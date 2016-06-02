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

#include "overlapping-compat.h"

module Servant.Server.Internal
  ( module Servant.Server.Internal
  , module Servant.Server.Internal.Context
  , module Servant.Server.Internal.BasicAuth
  , module Servant.Server.Internal.Router
  , module Servant.Server.Internal.RoutingApplication
  , module Servant.Server.Internal.ServantErr
  ) where

import           Control.Monad.Trans        (liftIO)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC8
import qualified Data.ByteString.Lazy       as BL
import           Data.Maybe                 (fromMaybe, mapMaybe)
import           Data.String                (fromString)
import           Data.String.Conversions    (cs, (<>))
import           Data.Typeable
import           GHC.TypeLits               (KnownNat, KnownSymbol, natVal,
                                             symbolVal)
import           Network.HTTP.Types         hiding (Header, ResponseHeaders)
import           Network.Socket             (SockAddr)
import           Network.Wai                (Application, Request, Response,
                                             httpVersion, isSecure,
                                             lazyRequestBody,
                                             rawQueryString, remoteHost,
                                             requestHeaders, requestMethod,
                                             responseLBS, vault)
import           Prelude                    ()
import           Prelude.Compat
import           Web.HttpApiData            (FromHttpApiData)
import           Web.HttpApiData.Internal   (parseHeaderMaybe,
                                             parseQueryParamMaybe,
                                             parseUrlPieceMaybe)

import           Servant.API                 ((:<|>) (..), (:>), BasicAuth, Capture,
                                              Verb, ReflectMethod(reflectMethod),
                                              IsSecure(..), Header,
                                              QueryFlag, QueryParam, QueryParams,
                                              Raw, RemoteHost, ReqBody, Vault,
                                              WithNamedContext)
import           Servant.API.ContentTypes    (AcceptHeader (..),
                                              AllCTRender (..),
                                              AllCTUnrender (..),
                                              AllMime,
                                              canHandleAcceptH)
import           Servant.API.ResponseHeaders (GetHeaders, Headers, getHeaders,
                                              getResponse)

import           Servant.Server.Internal.Context
import           Servant.Server.Internal.BasicAuth
import           Servant.Server.Internal.Router
import           Servant.Server.Internal.RoutingApplication
import           Servant.Server.Internal.ServantErr


class HasServer api context where
  type ServerT api (m :: * -> *) :: *

  route ::
       Proxy api
    -> Context context
    -> Delayed env (Server api)
    -> Router env

type Server api = ServerT api Handler

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
instance (HasServer a context, HasServer b context) => HasServer (a :<|> b) context where

  type ServerT (a :<|> b) m = ServerT a m :<|> ServerT b m

  route Proxy context server = choice (route pa context ((\ (a :<|> _) -> a) <$> server))
                                      (route pb context ((\ (_ :<|> b) -> b) <$> server))
    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b

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
-- >   where getBook :: Text -> Handler Book
-- >         getBook isbn = ...
instance (KnownSymbol capture, FromHttpApiData a, HasServer api context)
      => HasServer (Capture capture a :> api) context where

  type ServerT (Capture capture a :> api) m =
     a -> ServerT api m

  route Proxy context d =
    CaptureRouter $
        route (Proxy :: Proxy api)
              context
              (addCapture d $ \ txt -> case parseUrlPieceMaybe txt :: Maybe a of
                 Nothing -> delayedFail err400
                 Just v  -> return v
              )

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

methodCheck :: Method -> Request -> DelayedIO ()
methodCheck method request
  | allowedMethod method request = return ()
  | otherwise                    = delayedFail err405

-- This has switched between using 'Fail' and 'FailFatal' a number of
-- times. If the 'acceptCheck' is run after the body check (which would
-- be morally right), then we have to set this to 'FailFatal', because
-- the body check is not reversible, and therefore backtracking after the
-- body check is no longer an option. However, we now run the accept
-- check before the body check and can therefore afford to make it
-- recoverable.
acceptCheck :: (AllMime list) => Proxy list -> B.ByteString -> DelayedIO ()
acceptCheck proxy accH
  | canHandleAcceptH proxy (AcceptHeader accH) = return ()
  | otherwise                                  = delayedFail err406

methodRouter :: (AllCTRender ctypes a)
             => Method -> Proxy ctypes -> Status
             -> Delayed env (Handler a)
             -> Router env
methodRouter method proxy status action = leafRouter route'
  where
    route' env request respond =
          let accH = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
          in runAction (action `addMethodCheck` methodCheck method request
                               `addAcceptCheck` acceptCheck proxy accH
                       ) env request respond $ \ output -> do
               let handleA = handleAcceptH proxy (AcceptHeader accH) output
               processMethodRouter handleA status method Nothing request

methodRouterHeaders :: (GetHeaders (Headers h v), AllCTRender ctypes v)
                    => Method -> Proxy ctypes -> Status
                    -> Delayed env (Handler (Headers h v))
                    -> Router env
methodRouterHeaders method proxy status action = leafRouter route'
  where
    route' env request respond =
          let accH    = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
          in runAction (action `addMethodCheck` methodCheck method request
                               `addAcceptCheck` acceptCheck proxy accH
                       ) env request respond $ \ output -> do
                let headers = getHeaders output
                    handleA = handleAcceptH proxy (AcceptHeader accH) (getResponse output)
                processMethodRouter handleA status method (Just headers) request

instance OVERLAPPABLE_
         ( AllCTRender ctypes a, ReflectMethod method, KnownNat status
         ) => HasServer (Verb method status ctypes a) context where

  type ServerT (Verb method status ctypes a) m = m a

  route Proxy _ = methodRouter method (Proxy :: Proxy ctypes) status
    where method = reflectMethod (Proxy :: Proxy method)
          status = toEnum . fromInteger $ natVal (Proxy :: Proxy status)

instance OVERLAPPING_
         ( AllCTRender ctypes a, ReflectMethod method, KnownNat status
         , GetHeaders (Headers h a)
         ) => HasServer (Verb method status ctypes (Headers h a)) context where

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
-- >   deriving (Eq, Show, FromHttpApiData)
-- >
-- >            -- GET /view-my-referer
-- > type MyApi = "view-my-referer" :> Header "Referer" Referer :> Get '[JSON] Referer
-- >
-- > server :: Server MyApi
-- > server = viewReferer
-- >   where viewReferer :: Referer -> Handler referer
-- >         viewReferer referer = return referer
instance (KnownSymbol sym, FromHttpApiData a, HasServer api context)
      => HasServer (Header sym a :> api) context where

  type ServerT (Header sym a :> api) m =
    Maybe a -> ServerT api m

  route Proxy context subserver =
    let mheader req = parseHeaderMaybe =<< lookup str (requestHeaders req)
    in  route (Proxy :: Proxy api) context (passToServer subserver mheader)
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
-- >   where getBooksBy :: Maybe Text -> Handler [Book]
-- >         getBooksBy Nothing       = ...return all books...
-- >         getBooksBy (Just author) = ...return books by the given author...
instance (KnownSymbol sym, FromHttpApiData a, HasServer api context)
      => HasServer (QueryParam sym a :> api) context where

  type ServerT (QueryParam sym a :> api) m =
    Maybe a -> ServerT api m

  route Proxy context subserver =
    let querytext r = parseQueryText $ rawQueryString r
        param r =
          case lookup paramname (querytext r) of
            Nothing       -> Nothing -- param absent from the query string
            Just Nothing  -> Nothing -- param present with no value -> Nothing
            Just (Just v) -> parseQueryParamMaybe v -- if present, we try to convert to
                                        -- the right type
    in route (Proxy :: Proxy api) context (passToServer subserver param)
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
-- >   where getBooksBy :: [Text] -> Handler [Book]
-- >         getBooksBy authors = ...return all books by these authors...
instance (KnownSymbol sym, FromHttpApiData a, HasServer api context)
      => HasServer (QueryParams sym a :> api) context where

  type ServerT (QueryParams sym a :> api) m =
    [a] -> ServerT api m

  route Proxy context subserver =
    let querytext r = parseQueryText $ rawQueryString r
        -- if sym is "foo", we look for query string parameters
        -- named "foo" or "foo[]" and call parseQueryParam on the
        -- corresponding values
        parameters r = filter looksLikeParam (querytext r)
        values r = mapMaybe (convert . snd) (parameters r)
    in  route (Proxy :: Proxy api) context (passToServer subserver values)
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
-- >   where getBooks :: Bool -> Handler [Book]
-- >         getBooks onlyPublished = ...return all books, or only the ones that are already published, depending on the argument...
instance (KnownSymbol sym, HasServer api context)
      => HasServer (QueryFlag sym :> api) context where

  type ServerT (QueryFlag sym :> api) m =
    Bool -> ServerT api m

  route Proxy context subserver =
    let querytext r = parseQueryText $ rawQueryString r
        param r = case lookup paramname (querytext r) of
          Just Nothing  -> True  -- param is there, with no value
          Just (Just v) -> examine v -- param with a value
          Nothing       -> False -- param not in the query string
    in  route (Proxy :: Proxy api) context (passToServer subserver param)
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
instance HasServer Raw context where

  type ServerT Raw m = Application

  route Proxy _ rawApplication = RawRouter $ \ env request respond -> do
    r <- runDelayed rawApplication env request
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
-- >   where postBook :: Book -> Handler Book
-- >         postBook book = ...insert into your db...
instance ( AllCTUnrender list a, HasServer api context
         ) => HasServer (ReqBody list a :> api) context where

  type ServerT (ReqBody list a :> api) m =
    a -> ServerT api m

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (addBodyCheck subserver bodyCheck)
    where
      bodyCheck = withRequest $ \ request -> do
        -- See HTTP RFC 2616, section 7.2.1
        -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.2.1
        -- See also "W3C Internet Media Type registration, consistency of use"
        -- http://www.w3.org/2001/tag/2002/0129-mime
        let contentTypeH = fromMaybe "application/octet-stream"
                         $ lookup hContentType $ requestHeaders request
        mrqbody <- handleCTypeH (Proxy :: Proxy list) (cs contentTypeH)
               <$> liftIO (lazyRequestBody request)
        case mrqbody of
          Nothing        -> delayedFailFatal err415
          Just (Left e)  -> delayedFailFatal err400 { errBody = cs e }
          Just (Right v) -> return v

-- | Make sure the incoming request starts with @"/path"@, strip it and
-- pass the rest of the request path to @api@.
instance (KnownSymbol path, HasServer api context) => HasServer (path :> api) context where

  type ServerT (path :> api) m = ServerT api m

  route Proxy context subserver =
    pathRouter
      (cs (symbolVal proxyPath))
      (route (Proxy :: Proxy api) context subserver)
    where proxyPath = Proxy :: Proxy path

instance HasServer api context => HasServer (RemoteHost :> api) context where
  type ServerT (RemoteHost :> api) m = SockAddr -> ServerT api m

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (passToServer subserver remoteHost)

instance HasServer api context => HasServer (IsSecure :> api) context where
  type ServerT (IsSecure :> api) m = IsSecure -> ServerT api m

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (passToServer subserver secure)

    where secure req = if isSecure req then Secure else NotSecure

instance HasServer api context => HasServer (Vault :> api) context where
  type ServerT (Vault :> api) m = Vault -> ServerT api m

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (passToServer subserver vault)

instance HasServer api context => HasServer (HttpVersion :> api) context where
  type ServerT (HttpVersion :> api) m = HttpVersion -> ServerT api m

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (passToServer subserver httpVersion)

-- | Basic Authentication
instance ( KnownSymbol realm
         , HasServer api context
         , HasContextEntry context (BasicAuthCheck usr)
         )
    => HasServer (BasicAuth realm usr :> api) context where

  type ServerT (BasicAuth realm usr :> api) m = usr -> ServerT api m

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (subserver `addAuthCheck` authCheck)
    where
       realm = BC8.pack $ symbolVal (Proxy :: Proxy realm)
       basicAuthContext = getContextEntry context
       authCheck = withRequest $ \ req -> runBasicAuth req realm basicAuthContext

-- * helpers

ct_wildcard :: B.ByteString
ct_wildcard = "*" <> "/" <> "*" -- Because CPP

-- * General Authentication


-- * contexts

instance (HasContextEntry context (NamedContext name subContext), HasServer subApi subContext)
  => HasServer (WithNamedContext name subContext subApi) context where

  type ServerT (WithNamedContext name subContext subApi) m =
    ServerT subApi m

  route Proxy context delayed =
    route subProxy subContext delayed
    where
      subProxy :: Proxy subApi
      subProxy = Proxy

      subContext :: Context subContext
      subContext = descendIntoNamedContext (Proxy :: Proxy name) context
