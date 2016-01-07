{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

#include "overlapping-compat.h"

module Servant.Server.Internal
  ( module Servant.Server.Internal
  , module Servant.Server.Internal.Auth
  , module Servant.Server.Internal.Config
  , module Servant.Server.Internal.Router
  , module Servant.Server.Internal.RoutingApplication
  , module Servant.Server.Internal.ServantErr
  ) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative         ((<$>))
#endif
import           Control.Monad.Trans.Except                 (ExceptT, runExceptT)
import qualified Data.ByteString                            as B
import qualified Data.ByteString.Char8                      as BC8
import qualified Data.ByteString.Lazy                       as BL
import qualified Data.Map                                   as M
import           Data.Maybe                                 (fromMaybe,
                                                             mapMaybe)
import           Data.String                                (fromString)
import           Data.String.Conversions                    (cs, (<>))
import           Data.Text                                  (Text)
import           Data.Typeable
import           GHC.TypeLits               (KnownNat, KnownSymbol, natVal,
                                             symbolVal)
import           Network.HTTP.Types         hiding (Header, ResponseHeaders)
import           Network.Socket             (SockAddr)
import           Network.Wai                (Application, Request, Response,
                                             httpVersion, isSecure,
                                             lazyRequestBody, pathInfo,
                                             rawQueryString, remoteHost,
                                             requestHeaders, requestMethod,
                                             responseLBS, vault)
import           Web.HttpApiData            (FromHttpApiData)
import           Web.HttpApiData.Internal   (parseHeaderMaybe,
                                             parseQueryParamMaybe,
                                             parseUrlPieceMaybe)

import           Servant.API                 ((:<|>) (..), (:>), AuthProtect, BasicAuth, Capture,
                                              Verb, ReflectMethod(reflectMethod),
                                              IsSecure(..), Header,
                                              QueryFlag, QueryParam, QueryParams,
                                              Raw, RemoteHost, ReqBody, Vault,
                                              WithNamedConfig)
import           Servant.API.ContentTypes    (AcceptHeader (..),
                                              AllCTRender (..),
                                              AllCTUnrender (..),
                                              AllMime,
                                              canHandleAcceptH)
import           Servant.API.ResponseHeaders (GetHeaders, Headers, getHeaders,
                                              getResponse)

import           Servant.Server.Internal.Auth
import           Servant.Server.Internal.Config
import           Servant.Server.Internal.Router
import           Servant.Server.Internal.RoutingApplication
import           Servant.Server.Internal.ServantErr


class HasServer layout config where
  type ServerT layout (m :: * -> *) :: *

  route :: Proxy layout -> Config config -> Delayed (Server layout) -> Router

type Server layout = ServerT layout (ExceptT ServantErr IO)

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
instance (HasServer a config, HasServer b config) => HasServer (a :<|> b) config where

  type ServerT (a :<|> b) m = ServerT a m :<|> ServerT b m

  route Proxy config server = choice (route pa config ((\ (a :<|> _) -> a) <$> server))
                                     (route pb config ((\ (_ :<|> b) -> b) <$> server))
    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b

captured :: FromHttpApiData a => proxy (Capture sym a) -> Text -> Maybe a
captured _ = parseUrlPieceMaybe

-- | If you use 'Capture' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of the type specified by the 'Capture'.
-- This lets servant worry about getting it from the URL and turning
-- it into a value of the type you specify.
--
-- You can control how it'll be converted from 'Text' to your type
-- by simply providing an instance of 'FromHttpApiData' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> Capture "isbn" Text :> Get '[JSON] Book
-- >
-- > server :: Server MyApi
-- > server = getBook
-- >   where getBook :: Text -> ExceptT ServantErr IO Book
-- >         getBook isbn = ...
instance (KnownSymbol capture, FromHttpApiData a, HasServer sublayout config)
      => HasServer (Capture capture a :> sublayout) config where

  type ServerT (Capture capture a :> sublayout) m =
     a -> ServerT sublayout m

  route Proxy config d =
    DynamicRouter $ \ first ->
        route (Proxy :: Proxy sublayout)
              config
              (addCapture d $ case captured captureProxy first of
                 Nothing -> return $ Fail err404
                 Just v  -> return $ Route v
              )
    where
      captureProxy = Proxy :: Proxy (Capture capture a)

allowedMethodHead :: Method -> Request -> Bool
allowedMethodHead method request = method == methodGet && requestMethod request == methodHead

allowedMethod :: Method -> Request -> Bool
allowedMethod method request = allowedMethodHead method request || requestMethod request == method

processMethodRouter :: Maybe (BL.ByteString, BL.ByteString) -> Status -> Method
                    -> Maybe [(HeaderName, B.ByteString)]
                    -> Request -> RouteResult Response
processMethodRouter handleA status method headers request = case handleA of
  Nothing -> FailFatal err406 -- this should not happen (checked before), so we make it fatal if it does
  Just (contentT, body) -> Route $ responseLBS status hdrs bdy
    where
      bdy = if allowedMethodHead method request then "" else body
      hdrs = (hContentType, cs contentT) : (fromMaybe [] headers)

methodCheck :: Method -> Request -> IO (RouteResult ())
methodCheck method request
  | allowedMethod method request = return $ Route ()
  | otherwise                    = return $ Fail err405

acceptCheck :: (AllMime list) => Proxy list -> B.ByteString -> IO (RouteResult ())
acceptCheck proxy accH
  | canHandleAcceptH proxy (AcceptHeader accH) = return $ Route ()
  | otherwise                                  = return $ Fail err406

methodRouter :: (AllCTRender ctypes a)
             => Method -> Proxy ctypes -> Status
             -> Delayed (ExceptT ServantErr IO a)
             -> Router
methodRouter method proxy status action = LeafRouter route'
  where
    route' request respond
      | pathIsEmpty request =
          let accH = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
          in runAction (action `addMethodCheck` methodCheck method request
                               `addAcceptCheck` acceptCheck proxy accH
                       ) respond $ \ output -> do
               let handleA = handleAcceptH proxy (AcceptHeader accH) output
               processMethodRouter handleA status method Nothing request
      | otherwise = respond $ Fail err404

methodRouterHeaders :: (GetHeaders (Headers h v), AllCTRender ctypes v)
                    => Method -> Proxy ctypes -> Status
                    -> Delayed (ExceptT ServantErr IO (Headers h v))
                    -> Router
methodRouterHeaders method proxy status action = LeafRouter route'
  where
    route' request respond
      | pathIsEmpty request =
          let accH    = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
          in runAction (action `addMethodCheck` methodCheck method request
                               `addAcceptCheck` acceptCheck proxy accH
                       ) respond $ \ output -> do
                let headers = getHeaders output
                    handleA = handleAcceptH proxy (AcceptHeader accH) (getResponse output)
                processMethodRouter handleA status method (Just headers) request
      | otherwise = respond $ Fail err404

instance OVERLAPPABLE_
         ( AllCTRender ctypes a, ReflectMethod method, KnownNat status
         ) => HasServer (Verb method status ctypes a) config where

  type ServerT (Verb method status ctypes a) m = m a

  route Proxy _ = methodRouter method (Proxy :: Proxy ctypes) status
    where method = reflectMethod (Proxy :: Proxy method)
          status = toEnum . fromInteger $ natVal (Proxy :: Proxy status)

instance OVERLAPPING_
         ( AllCTRender ctypes a, ReflectMethod method, KnownNat status
         , GetHeaders (Headers h a)
         ) => HasServer (Verb method status ctypes (Headers h a)) config where

  type ServerT (Verb method status ctypes (Headers h a)) m = m (Headers h a)

  route Proxy _ = methodRouterHeaders method (Proxy :: Proxy ctypes) status
    where method = reflectMethod (Proxy :: Proxy method)
          status = toEnum . fromInteger $ natVal (Proxy :: Proxy status)

-- | If you use 'Header' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of the type specified by 'Header'.
-- This lets servant worry about extracting it from the request and turning
-- it into a value of the type you specify.
--
-- All it asks is for a 'FromHttpApiData' instance.
--
-- Example:
--
-- > newtype Referer = Referer Text
-- >   deriving (Eq, Show, FromHttpApiData, ToText)
-- >
-- >            -- GET /view-my-referer
-- > type MyApi = "view-my-referer" :> Header "Referer" Referer :> Get '[JSON] Referer
-- >
-- > server :: Server MyApi
-- > server = viewReferer
-- >   where viewReferer :: Referer -> ExceptT ServantErr IO referer
-- >         viewReferer referer = return referer
instance (KnownSymbol sym, FromHttpApiData a, HasServer sublayout config)
      => HasServer (Header sym a :> sublayout) config where

  type ServerT (Header sym a :> sublayout) m =
    Maybe a -> ServerT sublayout m

  route Proxy config subserver = WithRequest $ \ request ->
    let mheader = parseHeaderMaybe =<< lookup str (requestHeaders request)
    in  route (Proxy :: Proxy sublayout) config (passToServer subserver mheader)
    where str = fromString $ symbolVal (Proxy :: Proxy sym)

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
-- by simply providing an instance of 'FromHttpApiData' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> QueryParam "author" Text :> Get '[JSON] [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooksBy
-- >   where getBooksBy :: Maybe Text -> ExceptT ServantErr IO [Book]
-- >         getBooksBy Nothing       = ...return all books...
-- >         getBooksBy (Just author) = ...return books by the given author...
instance (KnownSymbol sym, FromHttpApiData a, HasServer sublayout config)
      => HasServer (QueryParam sym a :> sublayout) config where

  type ServerT (QueryParam sym a :> sublayout) m =
    Maybe a -> ServerT sublayout m

  route Proxy config subserver = WithRequest $ \ request ->
    let querytext = parseQueryText $ rawQueryString request
        param =
          case lookup paramname querytext of
            Nothing       -> Nothing -- param absent from the query string
            Just Nothing  -> Nothing -- param present with no value -> Nothing
            Just (Just v) -> parseQueryParamMaybe v -- if present, we try to convert to
                                        -- the right type
    in route (Proxy :: Proxy sublayout) config (passToServer subserver param)
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
-- by simply providing an instance of 'FromHttpApiData' for your type.
--
-- Example:
--
-- > type MyApi = "books" :> QueryParams "authors" Text :> Get '[JSON] [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooksBy
-- >   where getBooksBy :: [Text] -> ExceptT ServantErr IO [Book]
-- >         getBooksBy authors = ...return all books by these authors...
instance (KnownSymbol sym, FromHttpApiData a, HasServer sublayout config)
      => HasServer (QueryParams sym a :> sublayout) config where

  type ServerT (QueryParams sym a :> sublayout) m =
    [a] -> ServerT sublayout m

  route Proxy config subserver = WithRequest $ \ request ->
    let querytext = parseQueryText $ rawQueryString request
        -- if sym is "foo", we look for query string parameters
        -- named "foo" or "foo[]" and call parseQueryParam on the
        -- corresponding values
        parameters = filter looksLikeParam querytext
        values = mapMaybe (convert . snd) parameters
    in  route (Proxy :: Proxy sublayout) config (passToServer subserver values)
    where paramname = cs $ symbolVal (Proxy :: Proxy sym)
          looksLikeParam (name, _) = name == paramname || name == (paramname <> "[]")
          convert Nothing = Nothing
          convert (Just v) = parseQueryParamMaybe v

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
-- >   where getBooks :: Bool -> ExceptT ServantErr IO [Book]
-- >         getBooks onlyPublished = ...return all books, or only the ones that are already published, depending on the argument...
instance (KnownSymbol sym, HasServer sublayout config)
      => HasServer (QueryFlag sym :> sublayout) config where

  type ServerT (QueryFlag sym :> sublayout) m =
    Bool -> ServerT sublayout m

  route Proxy config subserver = WithRequest $ \ request ->
    let querytext = parseQueryText $ rawQueryString request
        param = case lookup paramname querytext of
          Just Nothing  -> True  -- param is there, with no value
          Just (Just v) -> examine v -- param with a value
          Nothing       -> False -- param not in the query string
    in  route (Proxy :: Proxy sublayout) config (passToServer subserver param)
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
instance HasServer Raw config where

  type ServerT Raw m = Application

  route Proxy _ rawApplication = LeafRouter $ \ request respond -> do
    r <- runDelayed rawApplication
    case r of
      Route app   -> app request (respond . Route)
      Fail a      -> respond $ Fail a
      FailFatal e -> respond $ FailFatal e

-- | If you use 'ReqBody' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of the type specified by 'ReqBody'.
-- The @Content-Type@ header is inspected, and the list provided is used to
-- attempt deserialization. If the request does not have a @Content-Type@
-- header, it is treated as @application/octet-stream@ (as specified in
-- <http://tools.ietf.org/html/rfc7231#section-3.1.1.5 RFC7231>.
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
-- >   where postBook :: Book -> ExceptT ServantErr IO Book
-- >         postBook book = ...insert into your db...
instance ( AllCTUnrender list a, HasServer sublayout config
         ) => HasServer (ReqBody list a :> sublayout) config where

  type ServerT (ReqBody list a :> sublayout) m =
    a -> ServerT sublayout m

  route Proxy config subserver = WithRequest $ \ request ->
    route (Proxy :: Proxy sublayout) config (addBodyCheck subserver (bodyCheck request))
    where
      bodyCheck request = do
        -- See HTTP RFC 2616, section 7.2.1
        -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.2.1
        -- See also "W3C Internet Media Type registration, consistency of use"
        -- http://www.w3.org/2001/tag/2002/0129-mime
        let contentTypeH = fromMaybe "application/octet-stream"
                         $ lookup hContentType $ requestHeaders request
        mrqbody <- handleCTypeH (Proxy :: Proxy list) (cs contentTypeH)
               <$> lazyRequestBody request
        case mrqbody of
          Nothing        -> return $ FailFatal err415
          Just (Left e)  -> return $ FailFatal err400 { errBody = cs e }
          Just (Right v) -> return $ Route v

-- | Make sure the incoming request starts with @"/path"@, strip it and
-- pass the rest of the request path to @sublayout@.
instance (KnownSymbol path, HasServer sublayout config) => HasServer (path :> sublayout) config where

  type ServerT (path :> sublayout) m = ServerT sublayout m

  route Proxy config subserver = StaticRouter $
    M.singleton (cs (symbolVal proxyPath))
                (route (Proxy :: Proxy sublayout) config subserver)
    where proxyPath = Proxy :: Proxy path

instance HasServer api config => HasServer (RemoteHost :> api) config where
  type ServerT (RemoteHost :> api) m = SockAddr -> ServerT api m

  route Proxy config subserver = WithRequest $ \req ->
    route (Proxy :: Proxy api) config (passToServer subserver $ remoteHost req)

instance HasServer api config => HasServer (IsSecure :> api) config where
  type ServerT (IsSecure :> api) m = IsSecure -> ServerT api m

  route Proxy config subserver = WithRequest $ \req ->
    route (Proxy :: Proxy api) config (passToServer subserver $ secure req)

    where secure req = if isSecure req then Secure else NotSecure

instance HasServer api config => HasServer (Vault :> api) config where
  type ServerT (Vault :> api) m = Vault -> ServerT api m

  route Proxy config subserver = WithRequest $ \req ->
    route (Proxy :: Proxy api) config (passToServer subserver $ vault req)

instance HasServer api config => HasServer (HttpVersion :> api) config where
  type ServerT (HttpVersion :> api) m = HttpVersion -> ServerT api m

  route Proxy config subserver = WithRequest $ \req ->
    route (Proxy :: Proxy api) config (passToServer subserver $ httpVersion req)

-- | Basic Authentication
instance (KnownSymbol realm, HasServer api config, HasConfigEntry config (BasicAuthCheck (AuthReturnType (BasicAuth realm))))
    => HasServer (BasicAuth realm :> api) config where
  type ServerT (BasicAuth realm :> api) m = AuthReturnType (BasicAuth realm) -> ServerT api m

  route Proxy config subserver = WithRequest $ \ request ->
    route (Proxy :: Proxy api) config (subserver `addAuthCheck` authCheck request)
    where
       realm = BC8.pack $ symbolVal (Proxy :: Proxy realm)
       basicAuthConfig = getConfigEntry config
       authCheck req = runBasicAuth req realm basicAuthConfig

-- | General Authentication
instance (HasServer api config, HasConfigEntry config (AuthHandler Request (AuthReturnType (AuthProtect tag)))) => HasServer (AuthProtect tag :> api) config where
  type ServerT (AuthProtect tag :> api) m = AuthReturnType (AuthProtect tag) -> ServerT api m

  route Proxy config subserver = WithRequest $ \ request ->
    route (Proxy :: Proxy api) config (subserver `addAuthCheck` authCheck request)
      where
        authHandler = unAuthHandler (getConfigEntry config)
        authCheck = fmap (either FailFatal Route) . runExceptT . authHandler

pathIsEmpty :: Request -> Bool
pathIsEmpty = go . pathInfo
  where go []   = True
        go [""] = True
        go _    = False

ct_wildcard :: B.ByteString
ct_wildcard = "*" <> "/" <> "*" -- Because CPP

-- * configs

instance (HasConfigEntry config (NamedConfig name subConfig), HasServer subApi subConfig)
  => HasServer (WithNamedConfig name subConfig subApi) config where

  type ServerT (WithNamedConfig name subConfig subApi) m =
    ServerT subApi m

  route Proxy config delayed =
    route subProxy subConfig delayed
    where
      subProxy :: Proxy subApi
      subProxy = Proxy

      subConfig :: Config subConfig
      subConfig = descendIntoNamedConfig (Proxy :: Proxy name) config
