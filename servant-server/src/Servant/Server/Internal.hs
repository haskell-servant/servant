{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

#if MIN_VERSION_base(4,9,0) && __GLASGOW_HASKELL__ >= 802
#define HAS_TYPE_ERROR
#endif

module Servant.Server.Internal
  ( module Servant.Server.Internal
  , module Servant.Server.Internal.BasicAuth
  , module Servant.Server.Internal.Context
  , module Servant.Server.Internal.Delayed
  , module Servant.Server.Internal.DelayedIO
  , module Servant.Server.Internal.ErrorFormatter
  , module Servant.Server.Internal.Handler
  , module Servant.Server.Internal.Router
  , module Servant.Server.Internal.RouteResult
  , module Servant.Server.Internal.RoutingApplication
  , module Servant.Server.Internal.ServerError
  ) where

import           Control.Monad
                 (join, when)
import           Control.Monad.Trans
                 (liftIO)
import           Control.Monad.Trans.Resource
                 (runResourceT)
import qualified Data.ByteString                            as B
import qualified Data.ByteString.Builder                    as BB
import qualified Data.ByteString.Char8                      as BC8
import qualified Data.ByteString.Lazy                       as BL
import           Data.Either
                 (partitionEithers)
import           Data.Maybe
                 (fromMaybe, isNothing, mapMaybe, maybeToList)
import           Data.Semigroup
                 ((<>))
import           Data.String
                 (IsString (..))
import           Data.String.Conversions
                 (cs)
import           Data.Tagged
                 (Tagged (..), retag, untag)
import qualified Data.Text                                  as T
import           Data.Typeable
import           GHC.TypeLits
                 (KnownNat, KnownSymbol, natVal, symbolVal)
import qualified Network.HTTP.Media                         as NHM
import           Network.HTTP.Types                         hiding
                 (Header, ResponseHeaders)
import           Network.Socket
                 (SockAddr)
import           Network.Wai
                 (Application, Request, httpVersion, isSecure, lazyRequestBody,
                 queryString, remoteHost, requestBody, requestHeaders,
                 requestMethod, responseLBS, responseStream, vault)
import           Prelude ()
import           Prelude.Compat
import           Servant.API
                 ((:<|>) (..), (:>), Accept (..), BasicAuth, Capture',
                 CaptureAll, Description, EmptyAPI, FramingRender (..),
                 FramingUnrender (..), FromSourceIO (..), Header', If,
                 IsSecure (..), QueryFlag, QueryParam', QueryParams, Raw,
                 ReflectMethod (reflectMethod), RemoteHost, ReqBody',
                 SBool (..), SBoolI (..), SourceIO, Stream, StreamBody',
                 Summary, ToSourceIO (..), Vault, Verb, NoContentVerb,
                 WithNamedContext)
import           Servant.API.ContentTypes
                 (AcceptHeader (..), AllCTRender (..), AllCTUnrender (..),
                 AllMime, MimeRender (..), MimeUnrender (..), canHandleAcceptH,
                 NoContent)
import           Servant.API.Modifiers
                 (FoldLenient, FoldRequired, RequestArgument,
                 unfoldRequestArgument)
import           Servant.API.ResponseHeaders
                 (GetHeaders, Headers, getHeaders, getResponse)
import qualified Servant.Types.SourceT                      as S
import           Web.HttpApiData
                 (FromHttpApiData, parseHeader, parseQueryParam,
                 parseUrlPieces, parseUrlPiece)

import           Servant.Server.Internal.BasicAuth
import           Servant.Server.Internal.Context
import           Servant.Server.Internal.Delayed
import           Servant.Server.Internal.DelayedIO
import           Servant.Server.Internal.ErrorFormatter
import           Servant.Server.Internal.Handler
import           Servant.Server.Internal.Router
import           Servant.Server.Internal.RouteResult
import           Servant.Server.Internal.RoutingApplication
import           Servant.Server.Internal.ServerError

#ifdef HAS_TYPE_ERROR
import           GHC.TypeLits
                 (ErrorMessage (..), TypeError)
#endif

class HasServer api context where
  type ServerT api (m :: * -> *) :: *

  route ::
       Proxy api
    -> Context context
    -> Delayed env (Server api)
    -> Router env

  hoistServerWithContext
      :: Proxy api
      -> Proxy context
      -> (forall x. m x -> n x)
      -> ServerT api m
      -> ServerT api n

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

  -- | This is better than 'enter', as it's tailor made for 'HasServer'.
  hoistServerWithContext _ pc nt (a :<|> b) =
    hoistServerWithContext (Proxy :: Proxy a) pc nt a :<|>
    hoistServerWithContext (Proxy :: Proxy b) pc nt b

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
instance (KnownSymbol capture, FromHttpApiData a
         , HasServer api context, SBoolI (FoldLenient mods)
         , HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
         )
      => HasServer (Capture' mods capture a :> api) context where

  type ServerT (Capture' mods capture a :> api) m =
     If (FoldLenient mods) (Either String a) a -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context d =
    CaptureRouter $
        route (Proxy :: Proxy api)
              context
              (addCapture d $ \ txt -> withRequest $ \ request ->
                case ( sbool :: SBool (FoldLenient mods)
                     , parseUrlPiece txt :: Either T.Text a) of
                  (SFalse, Left e) -> delayedFail $ formatError rep request $ cs e
                  (SFalse, Right v) -> return v
                  (STrue, piece) -> return $ (either (Left . cs) Right) piece)
    where
      rep = typeRep (Proxy :: Proxy Capture')
      formatError = urlParseErrorFormatter $ getContextEntry (mkContextWithErrorFormatter context)

-- | If you use 'CaptureAll' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a
-- function that takes an argument of a list of the type specified by
-- the 'CaptureAll'. This lets servant worry about getting values from
-- the URL and turning them into values of the type you specify.
--
-- You can control how they'll be converted from 'Text' to your type
-- by simply providing an instance of 'FromHttpApiData' for your type.
--
-- Example:
--
-- > type MyApi = "src" :> CaptureAll "segments" Text :> Get '[JSON] SourceFile
-- >
-- > server :: Server MyApi
-- > server = getSourceFile
-- >   where getSourceFile :: [Text] -> Handler Book
-- >         getSourceFile pathSegments = ...
instance (KnownSymbol capture, FromHttpApiData a
         , HasServer api context
         , HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
         )
      => HasServer (CaptureAll capture a :> api) context where

  type ServerT (CaptureAll capture a :> api) m =
    [a] -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context d =
    CaptureAllRouter $
        route (Proxy :: Proxy api)
              context
              (addCapture d $ \ txts -> withRequest $ \ request ->
                case parseUrlPieces txts of
                   Left e  -> delayedFail $ formatError rep request $ cs e
                   Right v -> return v
              )
    where
      rep = typeRep (Proxy :: Proxy CaptureAll)
      formatError = urlParseErrorFormatter $ getContextEntry (mkContextWithErrorFormatter context)

allowedMethodHead :: Method -> Request -> Bool
allowedMethodHead method request = method == methodGet && requestMethod request == methodHead

allowedMethod :: Method -> Request -> Bool
allowedMethod method request = allowedMethodHead method request || requestMethod request == method

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
acceptCheck :: (AllMime list) => Proxy list -> AcceptHeader -> DelayedIO ()
acceptCheck proxy accH
  | canHandleAcceptH proxy accH = return ()
  | otherwise                   = delayedFail err406

methodRouter :: (AllCTRender ctypes a)
             => (b -> ([(HeaderName, B.ByteString)], a))
             -> Method -> Proxy ctypes -> Status
             -> Delayed env (Handler b)
             -> Router env
methodRouter splitHeaders method proxy status action = leafRouter route'
  where
    route' env request respond =
          let accH = getAcceptHeader request
          in runAction (action `addMethodCheck` methodCheck method request
                               `addAcceptCheck` acceptCheck proxy accH
                       ) env request respond $ \ output -> do
               let (headers, b) = splitHeaders output
               case handleAcceptH proxy accH b of
                 Nothing -> FailFatal err406 -- this should not happen (checked before), so we make it fatal if it does
                 Just (contentT, body) ->
                      let bdy = if allowedMethodHead method request then "" else body
                      in Route $ responseLBS status ((hContentType, cs contentT) : headers) bdy

noContentRouter :: Method
             -> Status
             -> Delayed env (Handler b)
             -> Router env
noContentRouter method status action = leafRouter route'
  where
    route' env request respond =
          runAction (action `addMethodCheck` methodCheck method request)
                    env request respond $ \ _output ->
                      Route $ responseLBS status [] ""

instance {-# OVERLAPPABLE #-}
         ( AllCTRender ctypes a, ReflectMethod method, KnownNat status
         ) => HasServer (Verb method status ctypes a) context where

  type ServerT (Verb method status ctypes a) m = m a
  hoistServerWithContext _ _ nt s = nt s

  route Proxy _ = methodRouter ([],) method (Proxy :: Proxy ctypes) status
    where method = reflectMethod (Proxy :: Proxy method)
          status = toEnum . fromInteger $ natVal (Proxy :: Proxy status)

instance {-# OVERLAPPING #-}
         ( AllCTRender ctypes a, ReflectMethod method, KnownNat status
         , GetHeaders (Headers h a)
         ) => HasServer (Verb method status ctypes (Headers h a)) context where

  type ServerT (Verb method status ctypes (Headers h a)) m = m (Headers h a)
  hoistServerWithContext _ _ nt s = nt s

  route Proxy _ = methodRouter (\x -> (getHeaders x, getResponse x)) method (Proxy :: Proxy ctypes) status
    where method = reflectMethod (Proxy :: Proxy method)
          status = toEnum . fromInteger $ natVal (Proxy :: Proxy status)

instance (ReflectMethod method) =>
         HasServer (NoContentVerb method) context where

  type ServerT (NoContentVerb method) m = m NoContent
  hoistServerWithContext _ _ nt s = nt s

  route Proxy _ = noContentRouter method status204
    where method = reflectMethod (Proxy :: Proxy method)

instance {-# OVERLAPPABLE #-}
         ( MimeRender ctype chunk, ReflectMethod method, KnownNat status,
           FramingRender framing, ToSourceIO chunk a
         ) => HasServer (Stream method status framing ctype a) context where

  type ServerT (Stream method status framing ctype a) m = m a
  hoistServerWithContext _ _ nt s = nt s

  route Proxy _ = streamRouter ([],) method status (Proxy :: Proxy framing) (Proxy :: Proxy ctype)
      where method = reflectMethod (Proxy :: Proxy method)
            status = toEnum . fromInteger $ natVal (Proxy :: Proxy status)


instance {-# OVERLAPPING #-}
         ( MimeRender ctype chunk, ReflectMethod method, KnownNat status,
           FramingRender framing, ToSourceIO chunk a,
           GetHeaders (Headers h a)
         ) => HasServer (Stream method status framing ctype (Headers h a)) context where

  type ServerT (Stream method status framing ctype (Headers h a)) m = m (Headers h a)
  hoistServerWithContext _ _ nt s = nt s

  route Proxy _ = streamRouter (\x -> (getHeaders x, getResponse x)) method status (Proxy :: Proxy framing) (Proxy :: Proxy ctype)
      where method = reflectMethod (Proxy :: Proxy method)
            status = toEnum . fromInteger $ natVal (Proxy :: Proxy status)


streamRouter :: forall ctype a c chunk env framing. (MimeRender ctype chunk, FramingRender framing, ToSourceIO chunk a) =>
                (c -> ([(HeaderName, B.ByteString)], a))
             -> Method
             -> Status
             -> Proxy framing
             -> Proxy ctype
             -> Delayed env (Handler c)
             -> Router env
streamRouter splitHeaders method status framingproxy ctypeproxy action = leafRouter $ \env request respond ->
          let AcceptHeader accH = getAcceptHeader request
              cmediatype = NHM.matchAccept [contentType ctypeproxy] accH
              accCheck = when (isNothing cmediatype) $ delayedFail err406
              contentHeader = (hContentType, NHM.renderHeader . maybeToList $ cmediatype)
          in runAction (action `addMethodCheck` methodCheck method request
                               `addAcceptCheck` accCheck
                       ) env request respond $ \ output ->
                let (headers, fa) = splitHeaders output
                    sourceT = toSourceIO fa
                    S.SourceT kStepLBS = framingRender framingproxy (mimeRender ctypeproxy :: chunk -> BL.ByteString) sourceT
                in Route $ responseStream status (contentHeader : headers) $ \write flush -> do
                    let loop S.Stop          = flush
                        loop (S.Error err)   = fail err -- TODO: throw better error
                        loop (S.Skip s)      = loop s
                        loop (S.Effect ms)   = ms >>= loop
                        loop (S.Yield lbs s) = do
                            write (BB.lazyByteString lbs)
                            flush
                            loop s

                    kStepLBS loop

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
instance
  (KnownSymbol sym, FromHttpApiData a, HasServer api context
  , SBoolI (FoldRequired mods), SBoolI (FoldLenient mods)
  , HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
  )
  => HasServer (Header' mods sym a :> api) context where
------
  type ServerT (Header' mods sym a :> api) m =
    RequestArgument mods a -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver = route (Proxy :: Proxy api) context $
      subserver `addHeaderCheck` withRequest headerCheck
    where
      rep = typeRep (Proxy :: Proxy Header')
      formatError = headerParseErrorFormatter $ getContextEntry (mkContextWithErrorFormatter context)

      headerName :: IsString n => n
      headerName = fromString $ symbolVal (Proxy :: Proxy sym)

      headerCheck :: Request -> DelayedIO (RequestArgument mods a)
      headerCheck req =
          unfoldRequestArgument (Proxy :: Proxy mods) errReq errSt mev
        where
          mev :: Maybe (Either T.Text a)
          mev = fmap parseHeader $ lookup headerName (requestHeaders req)

          errReq = delayedFailFatal $ formatError rep req
            $ "Header " <> headerName <> " is required"

          errSt e = delayedFailFatal $ formatError rep req
            $ cs $ "Error parsing header "
                    <> headerName
                    <> " failed: " <> e

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
instance
  ( KnownSymbol sym, FromHttpApiData a, HasServer api context
  , SBoolI (FoldRequired mods), SBoolI (FoldLenient mods)
  , HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
  )
  => HasServer (QueryParam' mods sym a :> api) context where
------
  type ServerT (QueryParam' mods sym a :> api) m =
    RequestArgument mods a -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver =
    let querytext = queryToQueryText . queryString
        paramname = cs $ symbolVal (Proxy :: Proxy sym)

        rep = typeRep (Proxy :: Proxy QueryParam')
        formatError = urlParseErrorFormatter $ getContextEntry (mkContextWithErrorFormatter context)

        parseParam :: Request -> DelayedIO (RequestArgument mods a)
        parseParam req =
            unfoldRequestArgument (Proxy :: Proxy mods) errReq errSt mev
          where
            mev :: Maybe (Either T.Text a)
            mev = fmap parseQueryParam $ join $ lookup paramname $ querytext req

            errReq = delayedFailFatal $ formatError rep req
              $ cs $ "Query parameter " <> paramname <> " is required"

            errSt e = delayedFailFatal $ formatError rep req
              $ cs $ "Error parsing query parameter "
                      <> paramname <> " failed: " <> e

        delayed = addParameterCheck subserver . withRequest $ \req ->
                    parseParam req

    in route (Proxy :: Proxy api) context delayed

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
instance (KnownSymbol sym, FromHttpApiData a, HasServer api context
         , HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters)
      => HasServer (QueryParams sym a :> api) context where

  type ServerT (QueryParams sym a :> api) m =
    [a] -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver = route (Proxy :: Proxy api) context $
      subserver `addParameterCheck` withRequest paramsCheck
    where
      rep = typeRep (Proxy :: Proxy QueryParams)
      formatError = urlParseErrorFormatter $ getContextEntry (mkContextWithErrorFormatter context)

      paramname = cs $ symbolVal (Proxy :: Proxy sym)
      paramsCheck req =
          case partitionEithers $ fmap parseQueryParam params of
              ([], parsed) -> return parsed
              (errs, _)    -> delayedFailFatal $ formatError rep req
                  $ cs $ "Error parsing query parameter(s) "
                         <> paramname <> " failed: "
                         <> T.intercalate ", " errs
        where
          params :: [T.Text]
          params = mapMaybe snd
                 . filter (looksLikeParam . fst)
                 . queryToQueryText
                 . queryString
                 $ req

          looksLikeParam name = name == paramname || name == (paramname <> "[]")

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

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver =
    let querytext = queryToQueryText . queryString
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

  type ServerT Raw m = Tagged m Application

  hoistServerWithContext _ _ _ = retag

  route Proxy _ rawApplication = RawRouter $ \ env request respond -> runResourceT $ do
    -- note: a Raw application doesn't register any cleanup
    -- but for the sake of consistency, we nonetheless run
    -- the cleanup once its done
    r <- runDelayed rawApplication env request
    liftIO $ go r request respond

    where go r request respond = case r of
            Route app   -> untag app request (respond . Route)
            Fail a      -> respond $ Fail a
            FailFatal e -> respond $ FailFatal e

-- | If you use 'ReqBody' in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of the type specified by 'ReqBody'.
-- The @Content-Type@ header is inspected, and the list provided is used to
-- attempt deserialization. If the request does not have a @Content-Type@
-- header, it is treated as @application/octet-stream@ (as specified in
-- [RFC 7231 section 3.1.1.5](http://tools.ietf.org/html/rfc7231#section-3.1.1.5)).
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
instance ( AllCTUnrender list a, HasServer api context, SBoolI (FoldLenient mods)
         , HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
         ) => HasServer (ReqBody' mods list a :> api) context where

  type ServerT (ReqBody' mods list a :> api) m =
    If (FoldLenient mods) (Either String a) a -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver
      = route (Proxy :: Proxy api) context $
          addBodyCheck subserver ctCheck bodyCheck
    where
      rep = typeRep (Proxy :: Proxy ReqBody')
      formatError = bodyParserErrorFormatter $ getContextEntry (mkContextWithErrorFormatter context)

      -- Content-Type check, we only lookup we can try to parse the request body
      ctCheck = withRequest $ \ request -> do
        -- See HTTP RFC 2616, section 7.2.1
        -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.2.1
        -- See also "W3C Internet Media Type registration, consistency of use"
        -- http://www.w3.org/2001/tag/2002/0129-mime
        let contentTypeH = fromMaybe "application/octet-stream"
                         $ lookup hContentType $ requestHeaders request
        case canHandleCTypeH (Proxy :: Proxy list) (cs contentTypeH) :: Maybe (BL.ByteString -> Either String a) of
          Nothing -> delayedFail err415
          Just f  -> return f

      -- Body check, we get a body parsing functions as the first argument.
      bodyCheck f = withRequest $ \ request -> do
        mrqbody <- f <$> liftIO (lazyRequestBody request)
        case sbool :: SBool (FoldLenient mods) of
          STrue -> return mrqbody
          SFalse -> case mrqbody of
            Left e  -> delayedFailFatal $ formatError rep request e
            Right v -> return v

instance
    ( FramingUnrender framing, FromSourceIO chunk a, MimeUnrender ctype chunk
    , HasServer api context
    ) => HasServer (StreamBody' mods framing ctype a :> api) context
  where
    type ServerT (StreamBody' mods framing ctype a :> api) m = a -> ServerT api m

    hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

    route Proxy context subserver = route (Proxy :: Proxy api) context $
        addBodyCheck subserver ctCheck bodyCheck
      where
        ctCheck :: DelayedIO (SourceIO chunk -> a)
        -- TODO: do content-type check
        ctCheck = return fromSourceIO

        bodyCheck :: (SourceIO chunk -> a) -> DelayedIO a
        bodyCheck fromRS = withRequest $ \req -> do
            let mimeUnrender'    = mimeUnrender (Proxy :: Proxy ctype) :: BL.ByteString -> Either String chunk
            let framingUnrender' = framingUnrender (Proxy :: Proxy framing) mimeUnrender' :: SourceIO B.ByteString ->  SourceIO chunk
            let body = requestBody req
            let rs = S.fromAction B.null body
            let rs' = fromRS $ framingUnrender' rs
            return rs'

-- | Make sure the incoming request starts with @"/path"@, strip it and
-- pass the rest of the request path to @api@.
instance (KnownSymbol path, HasServer api context) => HasServer (path :> api) context where

  type ServerT (path :> api) m = ServerT api m

  route Proxy context subserver =
    pathRouter
      (cs (symbolVal proxyPath))
      (route (Proxy :: Proxy api) context subserver)
    where proxyPath = Proxy :: Proxy path
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt s

instance HasServer api context => HasServer (RemoteHost :> api) context where
  type ServerT (RemoteHost :> api) m = SockAddr -> ServerT api m

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (passToServer subserver remoteHost)
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

instance HasServer api context => HasServer (IsSecure :> api) context where
  type ServerT (IsSecure :> api) m = IsSecure -> ServerT api m

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (passToServer subserver secure)
    where secure req = if isSecure req then Secure else NotSecure

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

instance HasServer api context => HasServer (Vault :> api) context where
  type ServerT (Vault :> api) m = Vault -> ServerT api m

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (passToServer subserver vault)
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

instance HasServer api context => HasServer (HttpVersion :> api) context where
  type ServerT (HttpVersion :> api) m = HttpVersion -> ServerT api m

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (passToServer subserver httpVersion)
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

-- | Ignore @'Summary'@ in server handlers.
instance HasServer api ctx => HasServer (Summary desc :> api) ctx where
  type ServerT (Summary desc :> api) m = ServerT api m

  route _ = route (Proxy :: Proxy api)
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt s

-- | Ignore @'Description'@ in server handlers.
instance HasServer api ctx => HasServer (Description desc :> api) ctx where
  type ServerT (Description desc :> api) m = ServerT api m

  route _ = route (Proxy :: Proxy api)
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt s

-- | Singleton type representing a server that serves an empty API.
data EmptyServer = EmptyServer deriving (Typeable, Eq, Show, Bounded, Enum)

-- | Server for `EmptyAPI`
emptyServer :: ServerT EmptyAPI m
emptyServer = Tagged EmptyServer

-- | The server for an `EmptyAPI` is `emptyAPIServer`.
--
-- > type MyApi = "nothing" :> EmptyApi
-- >
-- > server :: Server MyApi
-- > server = emptyAPIServer
instance HasServer EmptyAPI context where
  type ServerT EmptyAPI m = Tagged m EmptyServer

  route Proxy _ _ = StaticRouter mempty mempty

  hoistServerWithContext _ _ _ = retag

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

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

-- * helpers

ct_wildcard :: B.ByteString
ct_wildcard = "*" <> "/" <> "*" -- Because CPP

getAcceptHeader :: Request -> AcceptHeader
getAcceptHeader = AcceptHeader . fromMaybe ct_wildcard . lookup hAccept . requestHeaders

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

  hoistServerWithContext _ _ nt s = hoistServerWithContext (Proxy :: Proxy subApi) (Proxy :: Proxy subContext) nt s

-------------------------------------------------------------------------------
-- TypeError helpers
-------------------------------------------------------------------------------

#ifdef HAS_TYPE_ERROR
-- | This instance catches mistakes when there are non-saturated
-- type applications on LHS of ':>'.
--
-- >>> serve (Proxy :: Proxy (Capture "foo" :> Get '[JSON] Int)) (error "...")
-- ...
-- ...Expected something of kind Symbol or *, got: k -> l on the LHS of ':>'.
-- ...Maybe you haven't applied enough arguments to
-- ...Capture' '[] "foo"
-- ...
--
-- >>> undefined :: Server (Capture "foo" :> Get '[JSON] Int)
-- ...
-- ...Expected something of kind Symbol or *, got: k -> l on the LHS of ':>'.
-- ...Maybe you haven't applied enough arguments to
-- ...Capture' '[] "foo"
-- ...
--
instance TypeError (HasServerArrowKindError arr) => HasServer ((arr :: k -> l) :> api) context
  where
    type ServerT (arr :> api) m = TypeError (HasServerArrowKindError arr)
    -- it doesn't really matter what sub route we peak
    route _ _ _ = error "servant-server panic: impossible happened in HasServer (arr :> api)"
    hoistServerWithContext _ _ _ = id

-- Cannot have TypeError here, otherwise use of this symbol will error :)
type HasServerArrowKindError arr =
    'Text "Expected something of kind Symbol or *, got: k -> l on the LHS of ':>'."
    ':$$: 'Text "Maybe you haven't applied enough arguments to"
    ':$$: 'ShowType arr

-- | This instance prevents from accidentally using '->' instead of ':>'
--
-- >>> serve (Proxy :: Proxy (Capture "foo" Int -> Get '[JSON] Int)) (error "...")
-- ...
-- ...No instance HasServer (a -> b).
-- ...Maybe you have used '->' instead of ':>' between
-- ...Capture' '[] "foo" Int
-- ...and
-- ...Verb 'GET 200 '[JSON] Int
-- ...
--
-- >>> undefined :: Server (Capture "foo" Int -> Get '[JSON] Int)
-- ...
-- ...No instance HasServer (a -> b).
-- ...Maybe you have used '->' instead of ':>' between
-- ...Capture' '[] "foo" Int
-- ...and
-- ...Verb 'GET 200 '[JSON] Int
-- ...
--
instance TypeError (HasServerArrowTypeError a b) => HasServer (a -> b) context
  where
    type ServerT (a -> b) m = TypeError (HasServerArrowTypeError a b)
    route _ _ _ = error "servant-server panic: impossible happened in HasServer (a -> b)"
    hoistServerWithContext _ _ _ = id

type HasServerArrowTypeError a b =
    'Text "No instance HasServer (a -> b)."
    ':$$: 'Text "Maybe you have used '->' instead of ':>' between "
    ':$$: 'ShowType a
    ':$$: 'Text "and"
    ':$$: 'ShowType b
#endif

-- $setup
-- >>> import Servant
