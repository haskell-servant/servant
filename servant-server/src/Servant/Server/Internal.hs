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

module Servant.Server.Internal where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative         (Applicative, (<$>))
import           Data.Monoid                 (Monoid, mappend, mempty)
#endif
import           Control.Monad.Trans.Either  (EitherT, runEitherT)
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as BL
import           Data.IORef                  (newIORef, readIORef, writeIORef)
import           Data.List                   (unfoldr)
import qualified Data.Map                    as M
import           Data.Maybe                  (catMaybes, fromMaybe)
import           Data.String                 (fromString)
import           Data.String.Conversions     (cs, (<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Data.Typeable
import           GHC.TypeLits                (KnownSymbol, symbolVal)
import           Network.HTTP.Types          hiding (Header, ResponseHeaders)
import           Network.Wai                 (Application, Request, Response,
                                              ResponseReceived, lazyRequestBody,
                                              pathInfo, rawQueryString,
                                              requestBody, requestHeaders,
                                              requestMethod, responseLBS,
                                              strictRequestBody)
import           Servant.API                 ((:<|>) (..), (:>), Capture,
                                               Delete, Get, Header,
                                              MatrixFlag, MatrixParam, MatrixParams,
                                              Patch, Post, Put, QueryFlag,
                                              QueryParam, QueryParams, Raw,
                                              ReqBody)
import           Servant.API.ContentTypes    (AcceptHeader (..),
                                              AllCTRender (..),
                                              AllCTUnrender (..))
import           Servant.API.ResponseHeaders (Headers, getResponse, GetHeaders,
                                              getHeaders)
import           Servant.Common.Text         (FromText, fromText)

import           Servant.Server.Internal.ServantErr

-- | Internal representation of a router.
data Router =
    WithRequest   (Request -> Router)
      -- ^ current request is passed to the router
  | StaticRouter  (M.Map Text Router)
      -- ^ first path component used for lookup and removed afterwards
  | DynamicRouter (Text -> Router)
      -- ^ first path component used for lookup and removed afterwards
  | LeafRouter    RoutingApplication
      -- ^ to be used for routes that match an empty path
  | Choice        Router Router
      -- ^ left-biased choice between two routers

-- | Smart constructor for the choice between routers.
-- We currently optimize the following cases:
--
--   * Two static routers can be joined by joining their maps.
--   * Two dynamic routers can be joined by joining their codomains.
--   * Two 'WithRequest' routers can be joined by passing them
--     the same request and joining their codomains.
--   * A 'WithRequest' router can be joined with anything else by
--     passing the same request to both but ignoring it in the
--     component that does not need it.
--
choice :: Router -> Router -> Router
choice (StaticRouter table1) (StaticRouter table2) =
  StaticRouter (M.unionWith choice table1 table2)
choice (DynamicRouter fun1)  (DynamicRouter fun2)  =
  DynamicRouter (\ first -> choice (fun1 first) (fun2 first))
choice (WithRequest router1) (WithRequest router2) =
  WithRequest (\ request -> choice (router1 request) (router2 request))
choice (WithRequest router1) router2 =
  WithRequest (\ request -> choice (router1 request) router2)
choice router1 (WithRequest router2) =
  WithRequest (\ request -> choice router1 (router2 request))
choice router1 router2 = Choice router1 router2

-- | Interpret a router as an application.
runRouter :: Router -> RoutingApplication
runRouter (WithRequest router) request respond =
  runRouter (router request) request respond
runRouter (StaticRouter table) request respond =
  case processedPathInfo request of
    first : rest
      | Just router <- M.lookup first table
      -> let request' = request { pathInfo = rest }
         in  runRouter router request' respond
    _ -> respond $ failWith NotFound
runRouter (DynamicRouter fun)  request respond =
  case processedPathInfo request of
    first : rest
      -> let request' = request { pathInfo = rest }
         in  runRouter (fun first) request' respond
    _ -> respond $ failWith NotFound
runRouter (LeafRouter app)     request respond = app request respond
runRouter (Choice r1 r2)       request respond =
  runRouter r1 request $ \ mResponse1 ->
    if isMismatch mResponse1
      then runRouter r2 request $ \ mResponse2 ->
             respond (mResponse1 <> mResponse2)
      else respond mResponse1

data ReqBodyState = Uncalled
                  | Called !B.ByteString
                  | Done !B.ByteString


toApplication :: RoutingApplication -> Application
toApplication ra request respond = do
  reqBodyRef <- newIORef Uncalled
  -- We may need to consume the requestBody more than once.  In order to
  -- maintain the illusion that 'requestBody' works as expected,
  -- 'ReqBodyState' is introduced, and the complete body is memoized and
  -- returned as many times as requested with empty "Done" marker chunks in
  -- between.
  -- See https://github.com/haskell-servant/servant/issues/3
  let memoReqBody = do
          ior <- readIORef reqBodyRef
          case ior of
            Uncalled -> do
                r <- BL.toStrict <$> strictRequestBody request
                writeIORef reqBodyRef $ Done r
                return r
            Called bs -> do
                writeIORef reqBodyRef $ Done bs
                return bs
            Done bs -> do
                writeIORef reqBodyRef $ Called bs
                return B.empty

  ra request{ requestBody = memoReqBody } (routingRespond . routeResult)
 where
  routingRespond :: Either RouteMismatch Response -> IO ResponseReceived
  routingRespond (Left NotFound) =
    respond $ responseLBS notFound404 [] "not found"
  routingRespond (Left WrongMethod) =
    respond $ responseLBS methodNotAllowed405 [] "method not allowed"
  routingRespond (Left (InvalidBody err)) =
    respond $ responseLBS badRequest400 [] $ fromString $ "invalid request body: " ++ err
  routingRespond (Left UnsupportedMediaType) =
    respond $ responseLBS unsupportedMediaType415 [] "unsupported media type"
  routingRespond (Left (HttpError status body)) =
    respond $ responseLBS status [] $ fromMaybe (BL.fromStrict $ statusMessage status) body
  routingRespond (Right response) =
    respond response

-- Note that the ordering of the constructors has great significance! It
-- determines the Ord instance and, consequently, the monoid instance.
-- * Route mismatch
data RouteMismatch =
    NotFound           -- ^ the usual "not found" error
  | WrongMethod        -- ^ a more informative "you just got the HTTP method wrong" error
  | UnsupportedMediaType -- ^ request body has unsupported media type
  | InvalidBody String -- ^ an even more informative "your json request body wasn't valid" error
  | HttpError Status (Maybe BL.ByteString)  -- ^ an even even more informative arbitrary HTTP response code error.
  deriving (Eq, Ord, Show)

instance Monoid RouteMismatch where
  mempty = NotFound
  -- The following isn't great, since it picks @InvalidBody@ based on
  -- alphabetical ordering, but any choice would be arbitrary.
  --
  -- "As one judge said to the other, 'Be just and if you can't be just, be
  -- arbitrary'" -- William Burroughs
  mappend = max


-- | A wrapper around @'Either' 'RouteMismatch' a@.
newtype RouteResult a =
  RR { routeResult :: Either RouteMismatch a }
  deriving (Eq, Show, Functor, Applicative)

runAction :: IO (RouteResult (EitherT ServantErr IO a))
          -> (RouteResult Response -> IO r)
          -> (a -> RouteResult Response)
          -> IO r
runAction action respond k = do
  r <- action
  go r
  where
    go (RR (Right a))  = do
      e <- runEitherT a
      respond $ case e of
        Right x  -> k x
        Left err -> succeedWith $ responseServantErr err
    go (RR (Left err)) = respond $ failWith err

feedTo :: IO (RouteResult (a -> b)) -> a -> IO (RouteResult b)
feedTo f x = (($ x) <$>) <$> f

extractL :: RouteResult (a :<|> b) -> RouteResult a
extractL (RR (Right (a :<|> _))) = RR (Right a)
extractL (RR (Left err))         = RR (Left err)

extractR :: RouteResult (a :<|> b) -> RouteResult b
extractR (RR (Right (_ :<|> b))) = RR (Right b)
extractR (RR (Left err))         = RR (Left err)

failWith :: RouteMismatch -> RouteResult a
failWith = RR . Left

succeedWith :: a -> RouteResult a
succeedWith = RR . Right

isMismatch :: RouteResult a -> Bool
isMismatch (RR (Left _)) = True
isMismatch _             = False

-- | Like `null . pathInfo`, but works with redundant trailing slashes.
pathIsEmpty :: Request -> Bool
pathIsEmpty = f . processedPathInfo
  where
    f []   = True
    f [""] = True
    f _    = False

-- | If we get a `Right`, it has precedence over everything else.
--
-- This in particular means that if we could get several 'Right's,
-- only the first we encounter would be taken into account.
instance Monoid (RouteResult a) where
  mempty = RR $ Left mempty

  RR (Left x)  `mappend` RR (Left y)  = RR $ Left (x <> y)
  RR (Left _)  `mappend` RR (Right y) = RR $ Right y
  r            `mappend` _            = r

type RoutingApplication =
     Request -- ^ the request, the field 'pathInfo' may be modified by url routing
  -> (RouteResult Response -> IO ResponseReceived) -> IO ResponseReceived

splitMatrixParameters :: Text -> (Text, Text)
splitMatrixParameters = T.break (== ';')

parsePathInfo :: Request -> [Text]
parsePathInfo = filter (/= "") . mergePairs . map splitMatrixParameters . pathInfo
  where mergePairs = concat . unfoldr pairToList
        pairToList []          = Nothing
        pairToList ((a, b):xs) = Just ([a, b], xs)

-- | Returns a processed pathInfo from the request.
--
-- In order to handle matrix parameters in the request correctly, the raw pathInfo needs to be
-- processed, so routing works as intended. Therefor this function should be used to access
-- the pathInfo for routing purposes.
processedPathInfo :: Request -> [Text]
processedPathInfo r =
  case pinfo of
    (x:xs) | T.head x == ';' -> xs
    _                        -> pinfo
  where pinfo = parsePathInfo r

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


-- | If you have a 'Delete' endpoint in your API,
-- the handler for this endpoint is meant to delete
-- a resource.
--
-- The code of the handler will, just like
-- for 'Servant.API.Get.Get', 'Servant.API.Post.Post' and
-- 'Servant.API.Put.Put', run in @EitherT ServantErr IO ()@.
-- The 'Int' represents the status code and the 'String' a message
-- to be returned. You can use 'Control.Monad.Trans.Either.left' to
-- painlessly error out if the conditions for a successful deletion
-- are not met.
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPABLE #-}
#endif
         ( AllCTRender ctypes a
         ) => HasServer (Delete ctypes a) where

  type ServerT (Delete ctypes a) m = m a

  route Proxy action = LeafRouter route'
    where
      route' request respond
        | pathIsEmpty request && requestMethod request == methodDelete = do
            runAction action respond $ \ output -> do
              let accH = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
              case handleAcceptH (Proxy :: Proxy ctypes) (AcceptHeader accH) output of
                Nothing -> failWith UnsupportedMediaType
                Just (contentT, body) -> succeedWith $
                  responseLBS status200 [ ("Content-Type" , cs contentT)] body
        | pathIsEmpty request && requestMethod request /= methodDelete =
            respond $ failWith WrongMethod
        | otherwise = respond $ failWith NotFound

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
         HasServer (Delete ctypes ()) where

  type ServerT (Delete ctypes ()) m = m ()

  route Proxy action = LeafRouter route'
    where
      route' request respond
        | pathIsEmpty request && requestMethod request == methodDelete = do
            runAction action respond $ \ () ->
              succeedWith $ responseLBS noContent204 [] ""
        | pathIsEmpty request && requestMethod request /= methodDelete =
            respond $ failWith WrongMethod
        | otherwise = respond $ failWith NotFound

-- Add response headers
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
         ( GetHeaders (Headers h v), AllCTRender ctypes v
         ) => HasServer (Delete ctypes (Headers h v)) where

  type ServerT (Delete ctypes (Headers h v)) m = m (Headers h v)

  route Proxy action = LeafRouter route'
    where
      route' request respond
        | pathIsEmpty request && requestMethod request == methodDelete = do
          runAction action respond $ \ output -> do
            let accH = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
                headers = getHeaders output
            case handleAcceptH (Proxy :: Proxy ctypes) (AcceptHeader accH) (getResponse output) of
              Nothing -> failWith UnsupportedMediaType
              Just (contentT, body) -> succeedWith $
                responseLBS status200 ( ("Content-Type" , cs contentT) : headers) body
        | pathIsEmpty request && requestMethod request /= methodDelete =
            respond $ failWith WrongMethod
        | otherwise = respond $ failWith NotFound

-- | When implementing the handler for a 'Get' endpoint,
-- just like for 'Servant.API.Delete.Delete', 'Servant.API.Post.Post'
-- and 'Servant.API.Put.Put', the handler code runs in the
-- @EitherT ServantErr IO@ monad, where the 'Int' represents
-- the status code and the 'String' a message, returned in case of
-- failure. You can quite handily use 'Control.Monad.Trans.EitherT.left'
-- to quickly fail if some conditions are not met.
--
-- If successfully returning a value, we use the type-level list, combined
-- with the request's @Accept@ header, to encode the value for you
-- (returning a status code of 200). If there was no @Accept@ header or it
-- was @*\/\*@, we return encode using the first @Content-Type@ type on the
-- list.
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPABLE #-}
#endif
         ( AllCTRender ctypes a ) => HasServer (Get ctypes a) where

  type ServerT (Get ctypes a) m = m a

  route Proxy action = LeafRouter route'
    where
      route' request respond
        | pathIsEmpty request && requestMethod request == methodGet = do
            runAction action respond $ \ output -> do
              let accH = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
              case handleAcceptH (Proxy :: Proxy ctypes) (AcceptHeader accH) output of
                Nothing -> failWith UnsupportedMediaType
                Just (contentT, body) -> succeedWith $
                  responseLBS ok200 [ ("Content-Type" , cs contentT)] body
        | pathIsEmpty request && requestMethod request /= methodGet =
            respond $ failWith WrongMethod
        | otherwise = respond $ failWith NotFound

-- '()' ==> 204 No Content
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
          HasServer (Get ctypes ()) where

  type ServerT (Get ctypes ()) m = m ()

  route Proxy action = LeafRouter route'
    where
      route' request respond
        | pathIsEmpty request && requestMethod request == methodGet = do
            runAction action respond $ \ () ->
              succeedWith $ responseLBS noContent204 [] ""
        | pathIsEmpty request && requestMethod request /= methodGet =
            respond $ failWith WrongMethod
        | otherwise = respond $ failWith NotFound

-- Add response headers
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
          ( GetHeaders (Headers h v), AllCTRender ctypes v
          ) => HasServer (Get ctypes (Headers h v)) where

  type ServerT (Get ctypes (Headers h v)) m = m (Headers h v)

  route Proxy action = LeafRouter route'
    where
      route' request respond
        | pathIsEmpty request && requestMethod request == methodGet = do
          runAction action respond $ \ output -> do
            let accH = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
                headers = getHeaders output
            case handleAcceptH (Proxy :: Proxy ctypes) (AcceptHeader accH) (getResponse output) of
              Nothing -> failWith UnsupportedMediaType
              Just (contentT, body) -> succeedWith $
                responseLBS ok200 ( ("Content-Type" , cs contentT) : headers) body
        | pathIsEmpty request && requestMethod request /= methodGet =
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

-- | When implementing the handler for a 'Post' endpoint,
-- just like for 'Servant.API.Delete.Delete', 'Servant.API.Get.Get'
-- and 'Servant.API.Put.Put', the handler code runs in the
-- @EitherT ServantErr IO@ monad, where the 'Int' represents
-- the status code and the 'String' a message, returned in case of
-- failure. You can quite handily use 'Control.Monad.Trans.EitherT.left'
-- to quickly fail if some conditions are not met.
--
-- If successfully returning a value, we use the type-level list, combined
-- with the request's @Accept@ header, to encode the value for you
-- (returning a status code of 201). If there was no @Accept@ header or it
-- was @*\/\*@, we return encode using the first @Content-Type@ type on the
-- list.
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPABLE #-}
#endif
         ( AllCTRender ctypes a
         ) => HasServer (Post ctypes a) where

  type ServerT (Post ctypes a) m = m a

  route Proxy action = LeafRouter route'
    where
      route' request respond
        | pathIsEmpty request && requestMethod request == methodPost = do
            runAction action respond $ \ output -> do
              let accH = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
              case handleAcceptH (Proxy :: Proxy ctypes) (AcceptHeader accH) output of
                Nothing -> failWith UnsupportedMediaType
                Just (contentT, body) -> succeedWith $
                  responseLBS status201 [ ("Content-Type" , cs contentT)] body
        | pathIsEmpty request && requestMethod request /= methodPost =
            respond $ failWith WrongMethod
        | otherwise = respond $ failWith NotFound

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
         HasServer (Post ctypes ()) where

  type ServerT (Post ctypes ()) m = m ()

  route Proxy action = LeafRouter route'
    where
      route' request respond
        | pathIsEmpty request && requestMethod request == methodPost = do
            runAction action respond $ \ () ->
              succeedWith $ responseLBS noContent204 [] ""
        | pathIsEmpty request && requestMethod request /= methodPost =
            respond $ failWith WrongMethod
        | otherwise = respond $ failWith NotFound

-- Add response headers
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
         ( GetHeaders (Headers h v), AllCTRender ctypes v
         ) => HasServer (Post ctypes (Headers h v)) where

  type ServerT (Post ctypes (Headers h v)) m = m (Headers h v)

  route Proxy action = LeafRouter route'
    where
      route' request respond
        | pathIsEmpty request && requestMethod request == methodPost = do
          runAction action respond $ \ output -> do
            let accH = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
                headers = getHeaders output
            case handleAcceptH (Proxy :: Proxy ctypes) (AcceptHeader accH) (getResponse output) of
              Nothing -> failWith UnsupportedMediaType
              Just (contentT, body) -> succeedWith $
                responseLBS status201 ( ("Content-Type" , cs contentT) : headers) body
        | pathIsEmpty request && requestMethod request /= methodPost =
            respond $ failWith WrongMethod
        | otherwise = respond $ failWith NotFound

-- | When implementing the handler for a 'Put' endpoint,
-- just like for 'Servant.API.Delete.Delete', 'Servant.API.Get.Get'
-- and 'Servant.API.Post.Post', the handler code runs in the
-- @EitherT ServantErr IO@ monad, where the 'Int' represents
-- the status code and the 'String' a message, returned in case of
-- failure. You can quite handily use 'Control.Monad.Trans.EitherT.left'
-- to quickly fail if some conditions are not met.
--
-- If successfully returning a value, we use the type-level list, combined
-- with the request's @Accept@ header, to encode the value for you
-- (returning a status code of 200). If there was no @Accept@ header or it
-- was @*\/\*@, we return encode using the first @Content-Type@ type on the
-- list.
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPABLE #-}
#endif
         ( AllCTRender ctypes a) => HasServer (Put ctypes a) where

  type ServerT (Put ctypes a) m = m a

  route Proxy action = LeafRouter route'
    where
      route' request respond
        | pathIsEmpty request && requestMethod request == methodPut = do
            runAction action respond $ \ output -> do
              let accH = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
              case handleAcceptH (Proxy :: Proxy ctypes) (AcceptHeader accH) output of
                Nothing -> failWith UnsupportedMediaType
                Just (contentT, body) -> succeedWith $
                  responseLBS status200 [ ("Content-Type" , cs contentT)] body
        | pathIsEmpty request && requestMethod request /= methodPut =
            respond $ failWith WrongMethod
        | otherwise = respond $ failWith NotFound

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
         HasServer (Put ctypes ()) where

  type ServerT (Put ctypes ()) m = m ()

  route Proxy action = LeafRouter route'
    where
      route' request respond
        | pathIsEmpty request && requestMethod request == methodPut = do
            runAction action respond $ \ () ->
              succeedWith $ responseLBS noContent204 [] ""
        | pathIsEmpty request && requestMethod request /= methodPut =
            respond $ failWith WrongMethod
        | otherwise = respond $ failWith NotFound

-- Add response headers
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
         ( GetHeaders (Headers h v), AllCTRender ctypes v
         ) => HasServer (Put ctypes (Headers h v)) where

  type ServerT (Put ctypes (Headers h v)) m = m (Headers h v)

  route Proxy action = LeafRouter route'
    where
      route' request respond
        | pathIsEmpty request && requestMethod request == methodPut = do
          runAction action respond $ \ output -> do
            let accH = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
                headers = getHeaders output
            case handleAcceptH (Proxy :: Proxy ctypes) (AcceptHeader accH) (getResponse output) of
              Nothing -> failWith UnsupportedMediaType
              Just (contentT, body) -> succeedWith $
                responseLBS status200 ( ("Content-Type" , cs contentT) : headers) body
        | pathIsEmpty request && requestMethod request /= methodPut =
            respond $ failWith WrongMethod
        | otherwise = respond $ failWith NotFound

-- | When implementing the handler for a 'Patch' endpoint,
-- just like for 'Servant.API.Delete.Delete', 'Servant.API.Get.Get'
-- and 'Servant.API.Put.Put', the handler code runs in the
-- @EitherT ServantErr IO@ monad, where the 'Int' represents
-- the status code and the 'String' a message, returned in case of
-- failure. You can quite handily use 'Control.Monad.Trans.EitherT.left'
-- to quickly fail if some conditions are not met.
--
-- If successfully returning a value, we just require that its type has
-- a 'ToJSON' instance and servant takes care of encoding it for you,
-- yielding status code 200 along the way.
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPABLE #-}
#endif
         ( AllCTRender ctypes a) => HasServer (Patch ctypes a) where

  type ServerT (Patch ctypes a) m = m a

  route Proxy action = LeafRouter route'
    where
      route' request respond
        | pathIsEmpty request && requestMethod request == methodPatch = do
            runAction action respond $ \ output -> do
              let accH = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
              case handleAcceptH (Proxy :: Proxy ctypes) (AcceptHeader accH) output of
                Nothing -> failWith UnsupportedMediaType
                Just (contentT, body) -> succeedWith $
                  responseLBS status200 [ ("Content-Type" , cs contentT)] body
        | pathIsEmpty request && requestMethod request /= methodPatch =
            respond $ failWith WrongMethod
        | otherwise = respond $ failWith NotFound

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
          HasServer (Patch ctypes ()) where

  type ServerT (Patch ctypes ()) m = m ()

  route Proxy action = LeafRouter route'
    where
      route' request respond
        | pathIsEmpty request && requestMethod request == methodPatch = do
            runAction action respond $ \ () ->
              succeedWith $ responseLBS noContent204 [] ""
        | pathIsEmpty request && requestMethod request /= methodPatch =
            respond $ failWith WrongMethod
        | otherwise = respond $ failWith NotFound

-- Add response headers
instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
         ( GetHeaders (Headers h v), AllCTRender ctypes v
         ) => HasServer (Patch ctypes (Headers h v)) where

  type ServerT (Patch ctypes (Headers h v)) m = m (Headers h v)

  route Proxy action = LeafRouter route'
    where
      route' request respond
        | pathIsEmpty request && requestMethod request == methodPatch = do
          runAction action respond $ \ outpatch -> do
            let accH = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
                headers = getHeaders outpatch
            case handleAcceptH (Proxy :: Proxy ctypes) (AcceptHeader accH) (getResponse outpatch) of
              Nothing -> failWith UnsupportedMediaType
              Just (contentT, body) -> succeedWith $
                responseLBS status200 ( ("Content-Type" , cs contentT) : headers) body
        | pathIsEmpty request && requestMethod request /= methodPatch =
            respond $ failWith WrongMethod
        | otherwise = respond $ failWith NotFound

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
        values = catMaybes $ map (convert . snd) parameters
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
                  values = catMaybes $ map (convert . snd) parameters
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

ct_wildcard :: B.ByteString
ct_wildcard = "*" <> "/" <> "*" -- Because CPP
