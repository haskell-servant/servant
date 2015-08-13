{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
#if !MIN_VERSION_base(4,8,0)
{-# LANGUAGE OverlappingInstances #-}
#endif

module Servant.Server.Internal
  ( module Servant.Server.Internal
  , module Servant.Server.Internal.PathInfo
  , module Servant.Server.Internal.Router
  , module Servant.Server.Internal.RoutingApplication
  , module Servant.Server.Internal.ServantErr
  ) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative         ((<$>))
#endif
import           Control.Monad.Trans.Either  (EitherT)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Char8       as B8
import qualified Data.ByteString.Lazy        as BL
import qualified Data.Map                    as M
import           Data.Maybe                  (mapMaybe, fromMaybe)
import           Data.String                 (fromString)
import           Data.String.Conversions     (cs, (<>), ConvertibleStrings)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Data.Typeable
import           GHC.TypeLits                (KnownSymbol, symbolVal, KnownNat, natVal)
import           Network.HTTP.Types          hiding (Header, ResponseHeaders)
import           Network.Socket              (SockAddr)
import           Network.Wai                 (Application, lazyRequestBody,
                                              rawQueryString, requestHeaders,
                                              requestMethod, responseLBS, remoteHost,
                                              isSecure, vault, httpVersion, Response,
                                              Request)
import           Servant.API                 ((:<|>) (..), (:>), Capture, DefaultStatusCode,
                                              Header, HttpMethod, IsSecure(..),
                                              MatrixFlag, MatrixParam, MatrixParams,
                                              QueryFlag, QueryParam, QueryParams,
                                              Raw, RemoteHost, ReqBody, Vault)
import           Servant.API.ContentTypes    (AcceptHeader (..),
                                              AllCTRender (..),
                                              AllCTUnrender (..))
import           Servant.API.ResponseHeaders (Headers, getResponse, GetHeaders,
                                              getHeaders)
import           Servant.Common.Text         (FromText, fromText)

import           Servant.Server.Internal.PathInfo
import           Servant.Server.Internal.Router
import           Servant.Server.Internal.RoutingApplication
import           Servant.Server.Internal.ServantErr

class HasServer layout where
  type ServerT layout (m :: * -> *) :: *

  route :: Proxy layout -> IO (RouteResult (Server layout)) -> Router

type Server layout = ServerT layout (EitherT ServantErr IO)

-- * Instances

-- | A server for @a ':<|>' b@ first tries to match the request against the route
--   represented by @a@ and if it fails tries @b@. You must provide a request
--   handler for each route.
--
-- > type MyApi = "books" :> Get '[JSON] [Book] -- GET /books
-- >         :<|> "books" :> ReqBody Book :> Post '[JSON] Book -- POST /books
-- >
-- > server :: Server MyApi
-- > server = listAllBooks :<|> postBook
-- >   where listAllBooks = ...
-- >         postBook book = ...
instance (HasServer a, HasServer b) => HasServer (a :<|> b) where

  type ServerT (a :<|> b) m = ServerT a m :<|> ServerT b m

  route Proxy server = choice (route pa (extractL <$> server))
                              (route pb (extractR <$> server))
    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b

captured :: FromText a => proxy (Capture sym a) -> Text -> Maybe a
captured _ = fromText

-- | If you use 'Capture' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of the type specified by the 'Capture'.
-- This lets servant worry about getting it from the URL and turning
-- it into a value of the type you specify.
--
-- You can control how it'll be converted from 'Text' to your type
-- by simply providing an instance of 'FromText' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> Capture "isbn" Text :> Get '[JSON] Book
-- >
-- > server :: Server MyApi
-- > server = getBook
-- >   where getBook :: Text -> EitherT ServantErr IO Book
-- >         getBook isbn = ...
instance (KnownSymbol capture, FromText a, HasServer sublayout)
      => HasServer (Capture capture a :> sublayout) where

  type ServerT (Capture capture a :> sublayout) m =
     a -> ServerT sublayout m

  route Proxy subserver =
    DynamicRouter $ \ first ->
      route (Proxy :: Proxy sublayout)
            (case captured captureProxy first of
               Nothing  -> return $ failWith NotFound
               Just v   -> feedTo subserver v)
    where captureProxy = Proxy :: Proxy (Capture capture a)

allowedMethodHead :: Method -> Request -> Bool
allowedMethodHead method request = method == methodGet && requestMethod request == methodHead

allowedMethod :: Method -> Request -> Bool
allowedMethod method request = allowedMethodHead method request || requestMethod request == method

processMethodRouter :: forall a. ConvertibleStrings a B.ByteString
                    => Maybe (a, BL.ByteString) -> Status -> Method
                    -> Maybe [(HeaderName, B.ByteString)]
                    -> Request -> RouteResult Response
processMethodRouter handleA status method headers request = case handleA of
  Nothing -> failWith UnsupportedMediaType
  Just (contentT, body) -> succeedWith $ responseLBS status hdrs bdy
    where
      bdy = if allowedMethodHead method request then "" else body
      hdrs = (hContentType, cs contentT) : (fromMaybe [] headers)

methodRouter :: (AllCTRender ctypes a)
             => Method -> Proxy ctypes -> Status
             -> IO (RouteResult (EitherT ServantErr IO a))
             -> Router
methodRouter method proxy status action = LeafRouter route'
  where
    route' request respond
      | pathIsEmpty request && allowedMethod method request = do
          runAction action respond $ \ output -> do
            let accH = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
                handleA = handleAcceptH proxy (AcceptHeader accH) output
            processMethodRouter handleA status method Nothing request
      | pathIsEmpty request && requestMethod request /= method =
          respond $ failWith WrongMethod
      | otherwise = respond $ failWith NotFound

methodRouterHeaders :: (GetHeaders (Headers h v), AllCTRender ctypes v)
                    => Method -> Proxy ctypes -> Status
                    -> IO (RouteResult (EitherT ServantErr IO (Headers h v)))
                    -> Router
methodRouterHeaders method proxy status action = LeafRouter route'
  where
    route' request respond
      | pathIsEmpty request && allowedMethod method request = do
        runAction action respond $ \ output -> do
          let accH = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
              headers = getHeaders output
              handleA = handleAcceptH proxy (AcceptHeader accH) (getResponse output)
          processMethodRouter handleA status method (Just headers) request
      | pathIsEmpty request && requestMethod request /= method =
          respond $ failWith WrongMethod
      | otherwise = respond $ failWith NotFound

methodRouterEmpty :: Method
                  -> IO (RouteResult (EitherT ServantErr IO ()))
                  -> Router
methodRouterEmpty method action = LeafRouter route'
  where
    route' request respond
      | pathIsEmpty request && allowedMethod method request = do
          runAction action respond $ \ () ->
            succeedWith $ responseLBS noContent204 [] ""
      | pathIsEmpty request && requestMethod request /= method =
          respond $ failWith WrongMethod
      | otherwise = respond $ failWith NotFound

-- | If you use 'Header' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of the type specified by 'Header'.
-- This lets servant worry about extracting it from the request and turning
-- it into a value of the type you specify.
--
-- All it asks is for a 'FromText' instance.
--
-- Example:
--
-- > newtype Referer = Referer Text
-- >   deriving (Eq, Show, FromText, ToText)
-- >
-- >            -- GET /view-my-referer
-- > type MyApi = "view-my-referer" :> Header "Referer" Referer :> Get '[JSON] Referer
-- >
-- > server :: Server MyApi
-- > server = viewReferer
-- >   where viewReferer :: Referer -> EitherT ServantErr IO referer
-- >         viewReferer referer = return referer
instance (KnownSymbol sym, FromText a, HasServer sublayout)
      => HasServer (Header sym a :> sublayout) where

  type ServerT (Header sym a :> sublayout) m =
    Maybe a -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \ request ->
    let mheader = fromText . decodeUtf8 =<< lookup str (requestHeaders request)
    in  route (Proxy :: Proxy sublayout) (feedTo subserver mheader)
    where str = fromString $ symbolVal (Proxy :: Proxy sym)


-- | When implementing the handler for an 'HttpMethod' endpoint,
-- the handler code runs in the @EitherT ServantErr IO@ monad.
--
-- You can quite handily use 'Control.Monad.Trans.EitherT.left'
-- to quickly fail if some conditions are not met.
--
-- If successfully returning a value, we use the type-level list,
-- combined with the request's @Accept@ header, to encode the value
-- for you (returning the default status code for the method -
-- e.g. for GET the status code will be 200). If there was no @Accept@
-- header or it was @*\/\*@, we encode the result using the first
-- @Content-Type@ type in the list.
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPABLE #-}
#endif
         ( AllCTRender ctypes a
         , KnownSymbol method
         , KnownNat (DefaultStatusCode method)
         ) => HasServer (HttpMethod method ctypes a) where

  type ServerT (HttpMethod method ctypes a) m = m a

  route Proxy = methodRouter method (Proxy :: Proxy ctypes) status
    where 
      method = B8.pack $ symbolVal (Proxy :: Proxy method)
      status = toEnum $ fromInteger $ natVal (Proxy :: Proxy (DefaultStatusCode method))

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif 
        ( KnownSymbol method
        ) => HasServer (HttpMethod method ctypes ()) where

  type ServerT (HttpMethod method ctypes ()) m = m ()

  route Proxy = methodRouterEmpty method
    where 
      method = B8.pack $ symbolVal (Proxy :: Proxy method)


-- Add response headers
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
         ( GetHeaders (Headers h v)
         , AllCTRender ctypes v
         , KnownSymbol method
         , KnownNat (DefaultStatusCode method)
         ) => HasServer (HttpMethod method ctypes (Headers h v)) where

  type ServerT (HttpMethod method ctypes (Headers h v)) m = m (Headers h v)

  route Proxy = methodRouterHeaders method (Proxy :: Proxy ctypes) status
    where 
      method = B8.pack $ symbolVal (Proxy :: Proxy method)
      status = toEnum $ fromInteger $ natVal (Proxy :: Proxy (DefaultStatusCode method))


-- | If you use @'QueryParam' "author" Text@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type @'Maybe' 'Text'@.
--
-- This lets servant worry about looking it up in the query string
-- and turning it into a value of the type you specify, enclosed
-- in 'Maybe', because it may not be there and servant would then
-- hand you 'Nothing'.
--
-- You can control how it'll be converted from 'Text' to your type
-- by simply providing an instance of 'FromText' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> QueryParam "author" Text :> Get '[JSON] [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooksBy
-- >   where getBooksBy :: Maybe Text -> EitherT ServantErr IO [Book]
-- >         getBooksBy Nothing       = ...return all books...
-- >         getBooksBy (Just author) = ...return books by the given author...
instance (KnownSymbol sym, FromText a, HasServer sublayout)
      => HasServer (QueryParam sym a :> sublayout) where

  type ServerT (QueryParam sym a :> sublayout) m =
    Maybe a -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \ request ->
    let querytext = parseQueryText $ rawQueryString request
        param =
          case lookup paramname querytext of
            Nothing       -> Nothing -- param absent from the query string
            Just Nothing  -> Nothing -- param present with no value -> Nothing
            Just (Just v) -> fromText v -- if present, we try to convert to
                                        -- the right type
    in route (Proxy :: Proxy sublayout) (feedTo subserver param)
    where paramname = cs $ symbolVal (Proxy :: Proxy sym)

-- | If you use @'QueryParams' "authors" Text@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type @['Text']@.
--
-- This lets servant worry about looking up 0 or more values in the query string
-- associated to @authors@ and turning each of them into a value of
-- the type you specify.
--
-- You can control how the individual values are converted from 'Text' to your type
-- by simply providing an instance of 'FromText' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> QueryParams "authors" Text :> Get '[JSON] [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooksBy
-- >   where getBooksBy :: [Text] -> EitherT ServantErr IO [Book]
-- >         getBooksBy authors = ...return all books by these authors...
instance (KnownSymbol sym, FromText a, HasServer sublayout)
      => HasServer (QueryParams sym a :> sublayout) where

  type ServerT (QueryParams sym a :> sublayout) m =
    [a] -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \ request ->
    let querytext = parseQueryText $ rawQueryString request
        -- if sym is "foo", we look for query string parameters
        -- named "foo" or "foo[]" and call fromText on the
        -- corresponding values
        parameters = filter looksLikeParam querytext
        values = mapMaybe (convert . snd) parameters
    in  route (Proxy :: Proxy sublayout) (feedTo subserver values)
    where paramname = cs $ symbolVal (Proxy :: Proxy sym)
          looksLikeParam (name, _) = name == paramname || name == (paramname <> "[]")
          convert Nothing = Nothing
          convert (Just v) = fromText v

-- | If you use @'QueryFlag' "published"@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type 'Bool'.
--
-- Example:
--
-- > type MyApi = "books" :> QueryFlag "published" :> Get '[JSON] [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooks
-- >   where getBooks :: Bool -> EitherT ServantErr IO [Book]
-- >         getBooks onlyPublished = ...return all books, or only the ones that are already published, depending on the argument...
instance (KnownSymbol sym, HasServer sublayout)
      => HasServer (QueryFlag sym :> sublayout) where

  type ServerT (QueryFlag sym :> sublayout) m =
    Bool -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \ request ->
    let querytext = parseQueryText $ rawQueryString request
        param = case lookup paramname querytext of
          Just Nothing  -> True  -- param is there, with no value
          Just (Just v) -> examine v -- param with a value
          Nothing       -> False -- param not in the query string
    in  route (Proxy :: Proxy sublayout) (feedTo subserver param)
    where paramname = cs $ symbolVal (Proxy :: Proxy sym)
          examine v | v == "true" || v == "1" || v == "" = True
                    | otherwise = False

parseMatrixText :: B.ByteString -> QueryText
parseMatrixText = parseQueryText

-- | If you use @'MatrixParam' "author" Text@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type @'Maybe' 'Text'@.
--
-- This lets servant worry about looking it up in the query string
-- and turning it into a value of the type you specify, enclosed
-- in 'Maybe', because it may not be there and servant would then
-- hand you 'Nothing'.
--
-- You can control how it'll be converted from 'Text' to your type
-- by simply providing an instance of 'FromText' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> MatrixParam "author" Text :> Get [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooksBy
-- >   where getBooksBy :: Maybe Text -> EitherT ServantErr IO [Book]
-- >         getBooksBy Nothing       = ...return all books...
-- >         getBooksBy (Just author) = ...return books by the given author...
instance (KnownSymbol sym, FromText a, HasServer sublayout)
      => HasServer (MatrixParam sym a :> sublayout) where

  type ServerT (MatrixParam sym a :> sublayout) m =
    Maybe a -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \ request ->
    case parsePathInfo request of
      (first : _)
        -> do let querytext = parseMatrixText . encodeUtf8 $ T.tail first
                  param = case lookup paramname querytext of
                    Nothing       -> Nothing -- param absent from the query string
                    Just Nothing  -> Nothing -- param present with no value -> Nothing
                    Just (Just v) -> fromText v -- if present, we try to convert to
                                          -- the right type
              route (Proxy :: Proxy sublayout) (feedTo subserver param)
      _   -> route (Proxy :: Proxy sublayout) (feedTo subserver Nothing)

    where paramname = cs $ symbolVal (Proxy :: Proxy sym)

-- | If you use @'MatrixParams' "authors" Text@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type @['Text']@.
--
-- This lets servant worry about looking up 0 or more values in the query string
-- associated to @authors@ and turning each of them into a value of
-- the type you specify.
--
-- You can control how the individual values are converted from 'Text' to your type
-- by simply providing an instance of 'FromText' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> MatrixParams "authors" Text :> Get [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooksBy
-- >   where getBooksBy :: [Text] -> EitherT ServantErr IO [Book]
-- >         getBooksBy authors = ...return all books by these authors...
instance (KnownSymbol sym, FromText a, HasServer sublayout)
      => HasServer (MatrixParams sym a :> sublayout) where

  type ServerT (MatrixParams sym a :> sublayout) m =
    [a] -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \ request ->
    case parsePathInfo request of
      (first : _)
        -> do let matrixtext = parseMatrixText . encodeUtf8 $ T.tail first
                  -- if sym is "foo", we look for matrix parameters
                  -- named "foo" or "foo[]" and call fromText on the
                  -- corresponding values
                  parameters = filter looksLikeParam matrixtext
                  values = mapMaybe (convert . snd) parameters
              route (Proxy :: Proxy sublayout) (feedTo subserver values)
      _ -> route (Proxy :: Proxy sublayout) (feedTo subserver [])
    where paramname = cs $ symbolVal (Proxy :: Proxy sym)
          looksLikeParam (name, _) = name == paramname || name == (paramname <> "[]")
          convert Nothing = Nothing
          convert (Just v) = fromText v

-- | If you use @'MatrixFlag' "published"@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type 'Bool'.
--
-- Example:
--
-- > type MyApi = "books" :> MatrixFlag "published" :> Get [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooks
-- >   where getBooks :: Bool -> EitherT ServantErr IO [Book]
-- >         getBooks onlyPublished = ...return all books, or only the ones that are already published, depending on the argument...
instance (KnownSymbol sym, HasServer sublayout)
      => HasServer (MatrixFlag sym :> sublayout) where

  type ServerT (MatrixFlag sym :> sublayout) m =
    Bool -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \ request ->
    case parsePathInfo request of
      (first : _)
        -> do let matrixtext = parseMatrixText . encodeUtf8 $ T.tail first
                  param = case lookup paramname matrixtext of
                    Just Nothing  -> True  -- param is there, with no value
                    Just (Just v) -> examine v -- param with a value
                    Nothing       -> False -- param not in the query string
              route (Proxy :: Proxy sublayout) (feedTo subserver param)
      _ -> route (Proxy :: Proxy sublayout) (feedTo subserver False)
    where paramname = cs $ symbolVal (Proxy :: Proxy sym)
          examine v | v == "true" || v == "1" || v == "" = True
                    | otherwise = False

-- | Just pass the request to the underlying application and serve its response.
--
-- Example:
--
-- > type MyApi = "images" :> Raw
-- >
-- > server :: Server MyApi
-- > server = serveDirectory "/var/www/images"
instance HasServer Raw where

  type ServerT Raw m = Application

  route Proxy rawApplication = LeafRouter $ \ request respond -> do
    r <- rawApplication
    case r of
      RR (Left err)  -> respond $ failWith err
      RR (Right app) -> app request (respond . succeedWith)

-- | If you use 'ReqBody' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of the type specified by 'ReqBody'.
-- The @Content-Type@ header is inspected, and the list provided is used to
-- attempt deserialization. If the request does not have a @Content-Type@
-- header, it is treated as @application/octet-stream@.
-- This lets servant worry about extracting it from the request and turning
-- it into a value of the type you specify.
--
--
-- All it asks is for a 'FromJSON' instance.
--
-- Example:
--
-- > type MyApi = "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book
-- >
-- > server :: Server MyApi
-- > server = postBook
-- >   where postBook :: Book -> EitherT ServantErr IO Book
-- >         postBook book = ...insert into your db...
instance ( AllCTUnrender list a, HasServer sublayout
         ) => HasServer (ReqBody list a :> sublayout) where

  type ServerT (ReqBody list a :> sublayout) m =
    a -> ServerT sublayout m

  route Proxy subserver = WithRequest $ \ request ->
    route (Proxy :: Proxy sublayout) $ do
      -- See HTTP RFC 2616, section 7.2.1
      -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.2.1
      -- See also "W3C Internet Media Type registration, consistency of use"
      -- http://www.w3.org/2001/tag/2002/0129-mime
      let contentTypeH = fromMaybe "application/octet-stream"
                       $ lookup hContentType $ requestHeaders request
      mrqbody <- handleCTypeH (Proxy :: Proxy list) (cs contentTypeH)
             <$> lazyRequestBody request
      case mrqbody of
        Nothing -> return $ failWith $ UnsupportedMediaType
        Just (Left e) -> return $ failWith $ InvalidBody e
        Just (Right v) -> feedTo subserver v

-- | Make sure the incoming request starts with @"/path"@, strip it and
-- pass the rest of the request path to @sublayout@.
instance (KnownSymbol path, HasServer sublayout) => HasServer (path :> sublayout) where

  type ServerT (path :> sublayout) m = ServerT sublayout m

  route Proxy subserver = StaticRouter $
    M.singleton (cs (symbolVal proxyPath))
                (route (Proxy :: Proxy sublayout) subserver)
    where proxyPath = Proxy :: Proxy path

instance HasServer api => HasServer (RemoteHost :> api) where
  type ServerT (RemoteHost :> api) m = SockAddr -> ServerT api m

  route Proxy subserver = WithRequest $ \req ->
    route (Proxy :: Proxy api) (feedTo subserver $ remoteHost req)

instance HasServer api => HasServer (IsSecure :> api) where
  type ServerT (IsSecure :> api) m = IsSecure -> ServerT api m

  route Proxy subserver = WithRequest $ \req ->
    route (Proxy :: Proxy api) (feedTo subserver $ secure req)

    where secure req = if isSecure req then Secure else NotSecure

instance HasServer api => HasServer (Vault :> api) where
  type ServerT (Vault :> api) m = Vault -> ServerT api m

  route Proxy subserver = WithRequest $ \req ->
    route (Proxy :: Proxy api) (feedTo subserver $ vault req)

instance HasServer api => HasServer (HttpVersion :> api) where
  type ServerT (HttpVersion :> api) m = HttpVersion -> ServerT api m

  route Proxy subserver = WithRequest $ \req ->
    route (Proxy :: Proxy api) (feedTo subserver $ httpVersion req)

ct_wildcard :: B.ByteString
ct_wildcard = "*" <> "/" <> "*" -- Because CPP
