{-# LANGUAGE CPP #-}

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
  )
where

import Control.Applicative ((<|>))
import Control.Monad (join, unless, void, when)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Resource (ReleaseKey, runResourceT)
import Data.Acquire
import Data.Bifunctor (first)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy as BSL
import Data.Constraint (Constraint, Dict (..))
import Data.Either (partitionEithers)
import Data.Kind (Type)
import Data.Maybe (fromMaybe, isNothing, mapMaybe, maybeToList)
import Data.String (IsString (..))
import Data.Tagged (Tagged (..), retag, untag)
import qualified Data.Text as T
import Data.Typeable
import GHC.Generics
import GHC.TypeLits
  ( ErrorMessage (..)
  , KnownNat
  , KnownSymbol
  , TypeError
  , symbolVal
  )
import qualified Network.HTTP.Media as NHM
import Network.HTTP.Types hiding (Header, ResponseHeaders, statusCode)
import Network.Socket (SockAddr)
import Network.Wai
  ( Application
  , Request
  , RequestBodyLength (..)
  , Response
  , ResponseReceived
  , getRequestBodyChunk
  , httpVersion
  , isSecure
  , lazyRequestBody
  , queryString
  , remoteHost
  , requestBodyLength
  , requestHeaderHost
  , requestHeaders
  , requestMethod
  , responseStream
  , vault
  )
import Servant.API
  ( Accept (..)
  , BasicAuth
  , Capture'
  , CaptureAll
  , DeepQuery
  , Description
  , EmptyAPI
  , Fragment
  , FramingRender (..)
  , FramingUnrender (..)
  , FromSourceIO (..)
  , Header'
  , Host
  , If
  , IsSecure (..)
  , NamedRoutes
  , NoContentVerb
  , OperationId
  , QueryFlag
  , QueryParam'
  , QueryParams
  , QueryString
  , Raw
  , RawM
  , ReflectMethod (reflectMethod)
  , RemoteHost
  , ReqBody'
  , SBool (..)
  , SBoolI (..)
  , SourceIO
  , Stream
  , StreamBody'
  , Summary
  , ToSourceIO (..)
  , Vault
  , Verb
  , WithNamedContext
  , WithResource
  , (:<|>) (..)
  , (:>)
  )
import Servant.API.ContentTypes
  ( AcceptHeader (..)
  , AllCTRender (..)
  , AllCTUnrender (..)
  , AllMime
  , AllMimeRender
  , MimeRender (..)
  , MimeUnrender (..)
  , NoContent
  , canHandleAcceptH
  )
import Servant.API.Generic
  ( GServantProduct
  , GenericMode (..)
  , ToServant
  , ToServantApi
  , fromServant
  , toServant
  )
import Servant.API.Modifiers
  ( FoldLenient
  , FoldRequired
  , RequestArgument
  , unfoldRequestArgument
  )
import Servant.API.MultiVerb
import Servant.API.QueryString (FromDeepQuery (..))
import Servant.API.ResponseHeaders
  ( GetHeaders
  , Headers
  , getHeaders
  , getResponse
  )
import Servant.API.Status (KnownStatus, statusFromNat)
import Servant.API.TypeErrors
import Servant.API.TypeLevel (AtMostOneFragment, FragmentUnique)
import qualified Servant.Types.SourceT as S
import Web.HttpApiData
  ( FromHttpApiData
  , parseHeader
  , parseQueryParam
  , parseUrlPiece
  , parseUrlPieces
  )

import Servant.Server.Internal.BasicAuth
import Servant.Server.Internal.Context
import Servant.Server.Internal.Delayed
import Servant.Server.Internal.DelayedIO
import Servant.Server.Internal.ErrorFormatter
import Servant.Server.Internal.Handler
import Servant.Server.Internal.ResponseRender
import Servant.Server.Internal.RouteResult
import Servant.Server.Internal.Router
import Servant.Server.Internal.RoutingApplication
import Servant.Server.Internal.ServerError

class HasServer api context where
  -- | The type of a server for this API, given a monad to run effects in.
  --
  -- Note that the result kind is @*@, so it is /not/ a monad transformer, unlike
  -- what the @T@ in the name might suggest.
  type ServerT api (m :: Type -> Type) :: Type

  route
    :: Proxy api
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

  route Proxy context server =
    choice
      (route pa context ((\(a :<|> _) -> a) <$> server))
      (route pb context ((\(_ :<|> b) -> b) <$> server))
    where
      pa = Proxy :: Proxy a
      pb = Proxy :: Proxy b

  -- \| This is better than 'enter', as it's tailor made for 'HasServer'.
  hoistServerWithContext _ pc nt (a :<|> b) =
    hoistServerWithContext (Proxy :: Proxy a) pc nt a
      :<|> hoistServerWithContext (Proxy :: Proxy b) pc nt b

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
instance
  ( FromHttpApiData a
  , HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
  , HasServer api context
  , KnownSymbol capture
  , SBoolI (FoldLenient mods)
  , Typeable a
  )
  => HasServer (Capture' mods capture a :> api) context
  where
  type
    ServerT (Capture' mods capture a :> api) m =
      If (FoldLenient mods) (Either String a) a -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context d =
    CaptureRouter [hint] $
      route
        (Proxy :: Proxy api)
        context
        ( addCapture d $ \txt -> withRequest $ \request ->
            case ( sbool :: SBool (FoldLenient mods)
                 , parseUrlPiece txt :: Either T.Text a
                 ) of
              (SFalse, Left e) -> delayedFail $ formatError rep request $ T.unpack e
              (SFalse, Right v) -> pure v
              (STrue, piece) -> pure $ either (Left . T.unpack) Right piece
        )
    where
      rep = typeRep (Proxy :: Proxy Capture')
      formatError = urlParseErrorFormatter $ getContextEntry (mkContextWithErrorFormatter context)
      hint = CaptureHint (T.pack $ symbolVal $ Proxy @capture) (typeRep (Proxy :: Proxy a))

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
instance
  ( FromHttpApiData a
  , HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
  , HasServer api context
  , KnownSymbol capture
  , Typeable a
  )
  => HasServer (CaptureAll capture a :> api) context
  where
  type
    ServerT (CaptureAll capture a :> api) m =
      [a] -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context d =
    CaptureAllRouter [hint] $
      route
        (Proxy :: Proxy api)
        context
        ( addCapture d $ \txts -> withRequest $ \request ->
            case parseUrlPieces txts of
              Left e -> delayedFail $ formatError rep request $ T.unpack e
              Right v -> pure v
        )
    where
      rep = typeRep (Proxy :: Proxy CaptureAll)
      formatError = urlParseErrorFormatter $ getContextEntry (mkContextWithErrorFormatter context)
      hint = CaptureHint (T.pack $ symbolVal $ Proxy @capture) (typeRep (Proxy :: Proxy [a]))

-- | If you use 'WithResource' in one of the endpoints for your API Servant
-- will provide the handler for this endpoint an argument of the specified type.
-- The lifespan of this resource will be automatically managed by Servant. This
-- resource will be created before the handler starts and it will be destoyed
-- after it ends. A new resource is created for each request to the endpoint.

-- The creation and destruction are done using a 'Data.Acquire.Acquire'
-- provided via server 'Context'.
--
-- Example
--
-- > type MyApi = WithResource Handle :> "writeToFile" :> Post '[JSON] NoContent
-- >
-- > server :: Server MyApi
-- > server = writeToFile
-- >   where writeToFile :: (ReleaseKey, Handle) -> Handler NoContent
-- >         writeToFile (_, h) = hPutStrLn h "message"
--
-- In addition to the resource, the handler will also receive a 'ReleaseKey'
-- which can be used to deallocate the resource before the end of the request
-- if desired.

instance
  (HasContextEntry ctx (Acquire a), HasServer api ctx)
  => HasServer (WithResource a :> api) ctx
  where
  type ServerT (WithResource a :> api) m = (ReleaseKey, a) -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy @api) pc nt . s

  route Proxy context d = route (Proxy @api) context (d `addParameterCheck` allocateResource)
    where
      allocateResource :: DelayedIO (ReleaseKey, a)
      allocateResource = DelayedIO $ lift $ allocateAcquire (getContextEntry context)

allowedMethodHead :: Method -> Request -> Bool
allowedMethodHead method request = method == methodGet && requestMethod request == methodHead

allowedMethod :: Method -> Request -> Bool
allowedMethod method request = allowedMethodHead method request || requestMethod request == method

methodCheck :: Method -> Request -> DelayedIO ()
methodCheck method request
  | allowedMethod method request = pure ()
  | otherwise = delayedFail err405

-- This has switched between using 'Fail' and 'FailFatal' a number of
-- times. If the 'acceptCheck' is run after the body check (which would
-- be morally right), then we have to set this to 'FailFatal', because
-- the body check is not reversible, and therefore backtracking after the
-- body check is no longer an option. However, we now run the accept
-- check before the body check and can therefore afford to make it
-- recoverable.
acceptCheck :: AllMime list => Proxy list -> AcceptHeader -> DelayedIO ()
acceptCheck proxy accH
  | canHandleAcceptH proxy accH = pure ()
  | otherwise = delayedFail err406

instance
  {-# OVERLAPPABLE #-}
  ( AllCTRender ctypes a
  , AllMimeRender ctypes a
  , KnownStatus status
  , ReflectMethod method
  )
  => HasServer (Verb method status ctypes a) context
  where
  type ServerT (Verb method status ctypes a) m = m a
  hoistServerWithContext _ _ nt = nt

  route Proxy = route (Proxy @(MultiVerb method ctypes '[Respond status "" a] a))

instance
  {-# OVERLAPPING #-}
  ( AllCTRender ctypes a
  , AllMimeRender ctypes a
  , GetHeaders (Headers h a)
  , KnownStatus status
  , ReflectMethod method
  )
  => HasServer (Verb method status ctypes (Headers h a)) context
  where
  type ServerT (Verb method status ctypes (Headers h a)) m = m (Headers h a)
  hoistServerWithContext _ _ nt = nt

  route Proxy = route (Proxy @(MultiVerb method ctypes '[Respond status "" (Headers h a)] (Headers h a)))

instance
  ReflectMethod method
  => HasServer (NoContentVerb method) context
  where
  type ServerT (NoContentVerb method) m = m NoContent
  hoistServerWithContext _ _ nt = nt

  route Proxy ctx action =
    route (Proxy @(MultiVerb method '() '[RespondAs '() 204 "" ()] ())) ctx $
      fmap void action

instance
  {-# OVERLAPPABLE #-}
  ( FramingRender framing
  , KnownNat status
  , MimeRender ctype chunk
  , ReflectMethod method
  , ToSourceIO chunk a
  )
  => HasServer (Stream method status framing ctype a) context
  where
  type ServerT (Stream method status framing ctype a) m = m a
  hoistServerWithContext _ _ nt = nt

  route Proxy _ = streamRouter ([],) method status (Proxy :: Proxy framing) (Proxy :: Proxy ctype)
    where
      method = reflectMethod (Proxy :: Proxy method)
      status = statusFromNat (Proxy :: Proxy status)

instance
  {-# OVERLAPPING #-}
  ( FramingRender framing
  , GetHeaders (Headers h a)
  , KnownNat status
  , MimeRender ctype chunk
  , ReflectMethod method
  , ToSourceIO chunk a
  )
  => HasServer (Stream method status framing ctype (Headers h a)) context
  where
  type ServerT (Stream method status framing ctype (Headers h a)) m = m (Headers h a)
  hoistServerWithContext _ _ nt = nt

  route Proxy _ = streamRouter (\x -> (getHeaders x, getResponse x)) method status (Proxy :: Proxy framing) (Proxy :: Proxy ctype)
    where
      method = reflectMethod (Proxy :: Proxy method)
      status = statusFromNat (Proxy :: Proxy status)

streamRouter
  :: forall ctype a c chunk env framing
   . (FramingRender framing, MimeRender ctype chunk, ToSourceIO chunk a)
  => (c -> ([(HeaderName, B.ByteString)], a))
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
   in runAction
        ( action
            `addMethodCheck` methodCheck method request
            `addAcceptCheck` accCheck
        )
        env
        request
        respond
        $ \output ->
          let (headers, fa) = splitHeaders output
              sourceT = toSourceIO fa
              S.SourceT kStepLBS = framingRender framingproxy (mimeRender ctypeproxy :: chunk -> BSL.ByteString) sourceT
           in Route $ responseStream status (contentHeader : headers) $ \write flush -> do
                let loop S.Stop = flush
                    loop (S.Error err) = fail err -- TODO: throw better error
                    loop (S.Skip s) = loop s
                    loop (S.Effect ms) = ms >>= loop
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
  ( FromHttpApiData a
  , HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
  , HasServer api context
  , KnownSymbol sym
  , SBoolI (FoldLenient mods)
  , SBoolI (FoldRequired mods)
  )
  => HasServer (Header' mods sym a :> api) context
  where
  ------
  type
    ServerT (Header' mods sym a :> api) m =
      RequestArgument mods a -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver =
    route (Proxy :: Proxy api) context $
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
          mev = parseHeader <$> lookup headerName (requestHeaders req)

          errReq =
            delayedFailFatal $
              formatError rep req $
                "Header " <> headerName <> " is required"

          errSt e =
            delayedFailFatal $
              formatError rep req $
                T.unpack $
                  "Error parsing header "
                    <> headerName
                    <> " failed: "
                    <> e

instance
  ( HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
  , HasServer api context
  , KnownSymbol sym
  )
  => HasServer (Host sym :> api) context
  where
  type ServerT (Host sym :> api) m = ServerT api m

  hoistServerWithContext _ = hoistServerWithContext (Proxy :: Proxy api)

  route _ context (Delayed{..}) =
    route (Proxy :: Proxy api) context $
      let formatError =
            headerParseErrorFormatter $ getContextEntry $ mkContextWithErrorFormatter context
          rep = typeRep (Proxy :: Proxy Host)
          targetHost = symbolVal (Proxy :: Proxy sym)
          hostCheck :: DelayedIO ()
          hostCheck = withRequest $ \req ->
            case requestHeaderHost req of
              Just hostBytes ->
                let host = BC8.unpack hostBytes
                 in unless (host == targetHost) $
                      delayedFail $
                        formatError rep req $
                          "Invalid host: " ++ host
              _ -> delayedFail $ formatError rep req "Host header missing"
       in Delayed{headersD = headersD <* hostCheck, ..}

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
  ( FromHttpApiData a
  , HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
  , HasServer api context
  , KnownSymbol sym
  , SBoolI (FoldLenient mods)
  , SBoolI (FoldRequired mods)
  )
  => HasServer (QueryParam' mods sym a :> api) context
  where
  ------
  type
    ServerT (QueryParam' mods sym a :> api) m =
      RequestArgument mods a -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver =
    let querytext = queryToQueryText . queryString
        paramname = T.pack $ symbolVal (Proxy :: Proxy sym)

        rep = typeRep (Proxy :: Proxy QueryParam')
        formatError = urlParseErrorFormatter $ getContextEntry (mkContextWithErrorFormatter context)

        parseParam :: Request -> DelayedIO (RequestArgument mods a)
        parseParam req =
          unfoldRequestArgument (Proxy :: Proxy mods) errReq errSt mev
          where
            mev :: Maybe (Either T.Text a)
            mev = fmap parseQueryParam $ join $ lookup paramname $ querytext req

            errReq =
              delayedFailFatal $
                formatError rep req $
                  T.unpack $
                    "Query parameter " <> paramname <> " is required"

            errSt e =
              delayedFailFatal $
                formatError rep req $
                  T.unpack $
                    "Error parsing query parameter "
                      <> paramname
                      <> " failed: "
                      <> e

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
instance
  ( FromHttpApiData a
  , HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
  , HasServer api context
  , KnownSymbol sym
  )
  => HasServer (QueryParams sym a :> api) context
  where
  type
    ServerT (QueryParams sym a :> api) m =
      [a] -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver =
    route (Proxy :: Proxy api) context $
      subserver `addParameterCheck` withRequest paramsCheck
    where
      rep = typeRep (Proxy :: Proxy QueryParams)
      formatError = urlParseErrorFormatter $ getContextEntry (mkContextWithErrorFormatter context)

      paramname = T.pack $ symbolVal (Proxy :: Proxy sym)
      paramsCheck req =
        case partitionEithers $ fmap parseQueryParam params of
          ([], parsed) -> pure parsed
          (errs, _) ->
            delayedFailFatal $
              formatError rep req $
                T.unpack $
                  "Error parsing query parameter(s) "
                    <> paramname
                    <> " failed: "
                    <> T.intercalate ", " errs
        where
          params :: [T.Text]
          params =
            mapMaybe snd
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
instance
  (HasServer api context, KnownSymbol sym)
  => HasServer (QueryFlag sym :> api) context
  where
  type
    ServerT (QueryFlag sym :> api) m =
      Bool -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver =
    let querytext = queryToQueryText . queryString
        param r = case lookup paramname (querytext r) of
          Just Nothing -> True -- param is there, with no value
          Just (Just v) -> examine v -- param with a value
          Nothing -> False -- param not in the query string
     in route (Proxy :: Proxy api) context (passToServer subserver param)
    where
      paramname = T.pack $ symbolVal (Proxy :: Proxy sym)
      examine v
        | v == "true" || v == "1" || v == "" = True
        | otherwise = False

-- | If you use @'QueryString'@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type @Query@ (@[('ByteString', 'Maybe' 'ByteString')]@).
--
-- This lets you extract the whole query string. This is useful when the query string
-- can contain parameters with dynamic names, that you can't access with @'QueryParam'@.
--
-- Example:
--
-- > type MyApi = "books" :> QueryString :> Get '[JSON] [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooksBy
-- >   where getBooksBy :: Query -> Handler [Book]
-- >         getBooksBy filters = ...filter books based on the dynamic filters provided...
instance
  HasServer api context
  => HasServer (QueryString :> api) context
  where
  ------
  type
    ServerT (QueryString :> api) m =
      Query -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (passToServer subserver queryString)

-- | If you use @'DeepQuery' "symbol" a@ in one of the endpoints for your API,
-- this automatically requires your server-side handler to be a function
-- that takes an argument of type @a@.
--
-- This lets you extract an object from multiple parameters in the query string,
-- with its fields enclosed in brackets: `/books?filter[author][name]=value`. When
-- all the fields are known in advance, it can be done with @'QueryParam'@ (it can
-- still be tedious if you the object has many fields). When some fields are dynamic,
-- it cannot be done with @'QueryParam'.
--
-- The way the object is constructed from the extracted fields can be controlled by
-- providing an instance on @'FromDeepQuery'@
--
-- Example:
--
-- > type MyApi = "books" :> DeepQuery "filter" BookQuery :> Get '[JSON] [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooksBy
-- >   where getBooksBy :: BookQuery -> Handler [Book]
-- >         getBooksBy query = ...filter books based on the dynamic filters provided...
instance
  ( FromDeepQuery a
  , HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
  , HasServer api context
  , KnownSymbol sym
  )
  => HasServer (DeepQuery sym a :> api) context
  where
  ------
  type
    ServerT (DeepQuery sym a :> api) m =
      a -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver =
    route (Proxy :: Proxy api) context $
      subserver `addParameterCheck` withRequest paramsCheck
    where
      rep = typeRep (Proxy :: Proxy DeepQuery)
      formatError = urlParseErrorFormatter $ getContextEntry (mkContextWithErrorFormatter context)

      paramname = T.pack $ symbolVal (Proxy :: Proxy sym)
      paramsCheck req =
        let relevantParams :: [(T.Text, Maybe T.Text)]
            relevantParams =
              mapMaybe isRelevantParam
                . queryToQueryText
                . queryString
                $ req
            isRelevantParam (name, value) =
              (,value)
                <$> case T.stripPrefix paramname name of
                  Just "" -> Just ""
                  Just x | "[" `T.isPrefixOf` x -> Just x
                  _ -> Nothing
         in case fromDeepQuery =<< traverse parseDeepParam relevantParams of
              Left e ->
                delayedFailFatal $
                  formatError rep req $
                    T.unpack $
                      "Error parsing deep query parameter(s) "
                        <> paramname
                        <> T.pack " failed: "
                        <> T.pack e
              Right parsed -> pure parsed

parseDeepParam :: (T.Text, Maybe T.Text) -> Either String ([T.Text], Maybe T.Text)
parseDeepParam (paramname, value) =
  let parseParam "" = pure []
      parseParam n = reverse <$> go [] n
      go parsed remaining = case T.take 1 remaining of
        "[" -> case T.breakOn "]" remaining of
          (_, "") -> Left $ "Error parsing deep param, missing closing ']': " <> T.unpack remaining
          (name, "]") -> pure $ T.drop 1 name : parsed
          (name, remaining') -> case T.take 2 remaining' of
            "][" -> go (T.drop 1 name : parsed) (T.drop 1 remaining')
            _ -> Left $ "Error parsing deep param, incorrect brackets: " <> T.unpack remaining
        _ -> Left $ "Error parsing deep param, missing opening '[': " <> T.unpack remaining
   in (,value) <$> parseParam paramname

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

  route Proxy _ rawApplication = RawRouter $ \env request respond -> runResourceT $ do
    -- note: a Raw application doesn't register any cleanup
    -- but for the sake of consistency, we nonetheless run
    -- the cleanup once its done
    r <- runDelayed rawApplication env request
    liftIO $ go r request respond
    where
      go r request respond = case r of
        Route app -> untag app request (respond . Route)
        Fail a -> respond $ Fail a
        FailFatal e -> respond $ FailFatal e

-- | Just pass the request to the underlying application and serve its response.
--
-- Example:
--
-- > type MyApi = "images" :> Raw
-- >
-- > server :: Server MyApi
-- > server = serveDirectory "/var/www/images"
instance HasServer RawM context where
  type ServerT RawM m = Request -> (Response -> IO ResponseReceived) -> m ResponseReceived

  route
    :: Proxy RawM
    -> Context context
    -> Delayed env (Request -> (Response -> IO ResponseReceived) -> Handler ResponseReceived)
    -> Router env
  route _ _ handleDelayed = RawRouter $ \env request respond -> runResourceT $ do
    routeResult <- runDelayed handleDelayed env request
    let respond' = liftIO . respond
    liftIO $ case routeResult of
      Route handler ->
        runHandler (handler request (respond . Route))
          >>= \case
            Left e -> respond' $ FailFatal e
            Right a -> pure a
      Fail e -> respond' $ Fail e
      FailFatal e -> respond' $ FailFatal e

  hoistServerWithContext _ _ f srvM req respond = f (srvM req respond)

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
instance
  ( AllCTUnrender list a
  , HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters
  , HasServer api context
  , SBoolI (FoldLenient mods)
  , SBoolI (FoldRequired mods)
  )
  => HasServer (ReqBody' mods list a :> api) context
  where
  type
    ServerT (ReqBody' mods list a :> api) m =
      RequestArgument mods a -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver =
    route (Proxy :: Proxy api) context $
      addBodyCheck subserver ctCheck bodyCheck
    where
      rep = typeRep (Proxy :: Proxy ReqBody')
      formatError = bodyParserErrorFormatter $ getContextEntry (mkContextWithErrorFormatter context)

      -- Content-Type check, we only lookup we can try to parse the request body
      ctCheck = withRequest $ \request ->
        -- See HTTP RFC 2616, section 7.2.1
        -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.2.1
        -- See also "W3C Internet Media Type registration, consistency of use"
        -- http://www.w3.org/2001/tag/2002/0129-mime
        let contentTypeHMaybe = lookup hContentType $ requestHeaders request
            contentTypeH = fromMaybe "application/octet-stream" contentTypeHMaybe
            canHandleContentTypeH :: Maybe (BSL.ByteString -> Either String a)
            canHandleContentTypeH = canHandleCTypeH (Proxy :: Proxy list) (BSL.fromStrict contentTypeH)

            -- In case ReqBody' is Optional and neither request body nor Content-Type header was provided.
            noOptionalReqBody =
              case (sbool :: SBool (FoldRequired mods), contentTypeHMaybe, requestBodyLength request) of
                (SFalse, Nothing, KnownLength 0) -> Just . const $ Left "This value does not matter (it is ignored)"
                _ -> Nothing
         in case canHandleContentTypeH <|> noOptionalReqBody of
              Nothing -> delayedFail err415
              Just f -> pure f

      bodyCheck f = withRequest $ \request ->
        let
          hasReqBody =
            case requestBodyLength request of
              KnownLength 0 -> False
              _ -> True

          serverErr :: String -> ServerError
          serverErr = formatError rep request

          required = sbool :: SBool (FoldRequired mods)
          lenient = sbool :: SBool (FoldLenient mods)
         in
          ( liftIO (lazyRequestBody request)
              >>= ( case (required, lenient, hasReqBody) of
                      (STrue, STrue, _) -> pure . first T.pack
                      (STrue, SFalse, _) -> either (delayedFailFatal . serverErr) pure
                      (SFalse, STrue, False) -> pure . either (const Nothing) (Just . Right)
                      (SFalse, SFalse, False) -> pure . either (const Nothing) Just
                      (SFalse, STrue, True) -> pure . Just . first T.pack
                      (SFalse, SFalse, True) -> either (delayedFailFatal . serverErr) (pure . Just)
                  )
                . f
          )

instance
  ( FramingUnrender framing
  , FromSourceIO chunk a
  , HasServer api context
  , MimeUnrender ctype chunk
  )
  => HasServer (StreamBody' mods framing ctype a :> api) context
  where
  type ServerT (StreamBody' mods framing ctype a :> api) m = a -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver =
    route (Proxy :: Proxy api) context $
      addBodyCheck subserver ctCheck bodyCheck
    where
      ctCheck :: DelayedIO (SourceIO chunk -> IO a)
      -- TODO: do content-type check
      ctCheck = pure fromSourceIO

      bodyCheck :: (SourceIO chunk -> IO a) -> DelayedIO a
      bodyCheck fromRS = withRequest $ \req -> do
        let mimeUnrender' = mimeUnrender (Proxy :: Proxy ctype) :: BSL.ByteString -> Either String chunk
        let framingUnrender' = framingUnrender (Proxy :: Proxy framing) mimeUnrender' :: SourceIO B.ByteString -> SourceIO chunk
        let body = getRequestBodyChunk req
        let rs = S.fromAction B.null body
        liftIO $ fromRS $ framingUnrender' rs

-- | Make sure the incoming request starts with @"/path"@, strip it and
-- pass the rest of the request path to @api@.
instance (HasServer api context, KnownSymbol path) => HasServer (path :> api) context where
  type ServerT (path :> api) m = ServerT api m

  route Proxy context subserver =
    pathRouter
      (T.pack (symbolVal proxyPath))
      (route (Proxy :: Proxy api) context subserver)
    where
      proxyPath = Proxy :: Proxy path
  hoistServerWithContext _ = hoistServerWithContext (Proxy :: Proxy api)

instance HasServer api context => HasServer (RemoteHost :> api) context where
  type ServerT (RemoteHost :> api) m = SockAddr -> ServerT api m

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (passToServer subserver remoteHost)
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

instance HasServer api context => HasServer (IsSecure :> api) context where
  type ServerT (IsSecure :> api) m = IsSecure -> ServerT api m

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (passToServer subserver secure)
    where
      secure req = if isSecure req then Secure else NotSecure

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
  hoistServerWithContext _ = hoistServerWithContext (Proxy :: Proxy api)

-- | Ignore @'OperationId'@ in server handlers.
instance HasServer api ctx => HasServer (OperationId operationId :> api) ctx where
  type ServerT (OperationId operationId :> api) m = ServerT api m

  route _ = route (Proxy :: Proxy api)
  hoistServerWithContext _ = hoistServerWithContext (Proxy :: Proxy api)

-- | Ignore @'Description'@ in server handlers.
instance HasServer api ctx => HasServer (Description desc :> api) ctx where
  type ServerT (Description desc :> api) m = ServerT api m

  route _ = route (Proxy :: Proxy api)
  hoistServerWithContext _ = hoistServerWithContext (Proxy :: Proxy api)

-- | Singleton type representing a server that serves an empty API.
data EmptyServer = EmptyServer deriving (Bounded, Enum, Eq, Show, Typeable)

-- | Server for `EmptyAPI`
emptyServer :: ServerT EmptyAPI m
emptyServer = Tagged EmptyServer

-- | The server for an `EmptyAPI` is `emptyServer`.
--
-- > type MyApi = "nothing" :> EmptyApi
-- >
-- > server :: Server MyApi
-- > server = emptyServer
instance HasServer EmptyAPI context where
  type ServerT EmptyAPI m = Tagged m EmptyServer

  route Proxy _ _ = StaticRouter mempty mempty

  hoistServerWithContext _ _ _ = retag

-- | Ignore @'EmptyAPI'@ as part of route in server handlers.
instance HasServer api context => HasServer (EmptyAPI :> api) context where
  type ServerT (EmptyAPI :> api) m = ServerT api m

  route _ = route (Proxy :: Proxy api)
  hoistServerWithContext _ = hoistServerWithContext (Proxy :: Proxy api)

-- | Basic Authentication
instance
  ( HasContextEntry context (BasicAuthCheck usr)
  , HasServer api context
  , KnownSymbol realm
  )
  => HasServer (BasicAuth realm usr :> api) context
  where
  type ServerT (BasicAuth realm usr :> api) m = usr -> ServerT api m

  route Proxy context subserver =
    route (Proxy :: Proxy api) context (subserver `addAuthCheck` authCheck)
    where
      realm = BC8.pack $ symbolVal (Proxy :: Proxy realm)
      basicAuthContext = getContextEntry context
      authCheck = withRequest $ \req -> runBasicAuth req realm basicAuthContext

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

-- * helpers

getAcceptHeader :: Request -> AcceptHeader
getAcceptHeader = AcceptHeader . fromMaybe "*/*" . lookup hAccept . requestHeaders

-- * General Authentication

-- * contexts

instance
  (HasContextEntry context (NamedContext name subContext), HasServer subApi subContext)
  => HasServer (WithNamedContext name subContext subApi) context
  where
  type
    ServerT (WithNamedContext name subContext subApi) m =
      ServerT subApi m

  route Proxy context = route subProxy subContext
    where
      subProxy :: Proxy subApi
      subProxy = Proxy

      subContext :: Context subContext
      subContext = descendIntoNamedContext (Proxy :: Proxy name) context

  hoistServerWithContext _ _ = hoistServerWithContext (Proxy :: Proxy subApi) (Proxy :: Proxy subContext)

-------------------------------------------------------------------------------
-- Custom type errors
-------------------------------------------------------------------------------

-- Erroring instance for 'HasServer' when a combinator is not fully applied
instance
  TypeError
    ( PartialApplication
        @(Type -> [Type] -> Constraint)
        HasServer
        arr
    )
  => HasServer ((arr :: a -> b) :> sub) context
  where
  type ServerT (arr :> sub) _ = TypeError (PartialApplication (HasServer :: Type -> [Type] -> Constraint) arr)
  route = error "unreachable"
  hoistServerWithContext _ _ _ _ = error "unreachable"

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
instance TypeError (HasServerArrowTypeError a b) => HasServer (a -> b) context where
  type ServerT (a -> b) m = TypeError (HasServerArrowTypeError a b)
  route _ _ _ = error "servant-server panic: impossible happened in HasServer (a -> b)"
  hoistServerWithContext _ _ _ = id

type HasServerArrowTypeError a b =
  'Text "No instance HasServer (a -> b)."
    ':$$: 'Text "Maybe you have used '->' instead of ':>' between "
    ':$$: 'ShowType a
    ':$$: 'Text "and"
    ':$$: 'ShowType b

-- Erroring instances for 'HasServer' for unknown API combinators

-- XXX: This omits the @context@ parameter, e.g.:
--
-- "There is no instance for HasServer (Bool :> …)". Do we care ?
instance
  {-# OVERLAPPABLE #-}
  TypeError
    ( NoInstanceForSub
        @(Type -> [Type] -> Constraint)
        HasServer
        ty
    )
  => HasServer (ty :> sub) context

instance {-# OVERLAPPABLE #-} TypeError (NoInstanceFor (HasServer api context)) => HasServer api context

-- | Ignore @'Fragment'@ in server handlers.
-- See <https://ietf.org/rfc/rfc2616.html#section-15.1.3> for more details.
--
-- Example:
--
-- > type MyApi = "books" :> Fragment Text :> Get '[JSON] [Book]
-- >
-- > server :: Server MyApi
-- > server = getBooks
-- >   where getBooks :: Handler [Book]
-- >         getBooks = ...return all books...
instance
  (AtMostOneFragment api, FragmentUnique (Fragment a1 :> api), HasServer api context)
  => HasServer (Fragment a1 :> api) context
  where
  type ServerT (Fragment a1 :> api) m = ServerT api m

  route _ = route (Proxy :: Proxy api)

  hoistServerWithContext _ = hoistServerWithContext (Proxy :: Proxy api)

-- $setup
-- >>> import Servant

-- | A type that specifies that an API record contains a server implementation.
data AsServerT (m :: Type -> Type)

instance GenericMode (AsServerT m) where
  type AsServerT m :- api = ServerT api m

type AsServer = AsServerT Handler

-- | Set of constraints required to convert to / from vanilla server types.
type GServerConstraints api m =
  ( ToServant api (AsServerT m) ~ ServerT (ToServantApi api) m
  , GServantProduct (Rep (api (AsServerT m)))
  )

-- | This class is a necessary evil: in the implementation of 'HasServer' for
--  @'NamedRoutes' api@, we essentially need the quantified constraint @forall
--  m. 'GServerConstraints' m@ to hold.
--
-- We cannot require do that directly as the definition of 'GServerConstraints'
-- contains type family applications ('Rep' and 'ServerT'). The trick is to hide
-- those type family applications behind a typeclass providing evidence for
-- @'GServerConstraints' api m@ in the form of a dictionary, and require that
-- @forall m. 'GServer' api m@ instead.
--
-- Users shouldn't have to worry about this class, as the only possible instance
-- is provided in this module for all record APIs.
class GServer (api :: Type -> Type) (m :: Type -> Type) where
  gServerProof :: Dict (GServerConstraints api m)

instance
  ( GServantProduct (Rep (api (AsServerT m)))
  , ToServant api (AsServerT m) ~ ServerT (ToServantApi api) m
  )
  => GServer api m
  where
  gServerProof = Dict

instance
  ( ErrorIfNoGeneric api
  , HasServer (ToServantApi api) context
  , forall m. GServer api m
  , forall m. Generic (api (AsServerT m))
  )
  => HasServer (NamedRoutes api) context
  where
  type ServerT (NamedRoutes api) m = api (AsServerT m)

  route
    :: Proxy (NamedRoutes api)
    -> Context context
    -> Delayed env (api (AsServerT Handler))
    -> Router env
  route _ ctx delayed =
    case gServerProof @api @Handler of
      Dict -> route (Proxy @(ToServantApi api)) ctx (toServant <$> delayed)

  hoistServerWithContext
    :: forall m n
     . Proxy (NamedRoutes api)
    -> Proxy context
    -> (forall x. m x -> n x)
    -> api (AsServerT m)
    -> api (AsServerT n)
  hoistServerWithContext _ pctx nat server =
    case (gServerProof @api @m, gServerProof @api @n) of
      (Dict, Dict) ->
        fromServant servantSrvN
        where
          servantSrvM :: ServerT (ToServantApi api) m =
            toServant server
          servantSrvN :: ServerT (ToServantApi api) n =
            hoistServerWithContext (Proxy @(ToServantApi api)) pctx nat servantSrvM

instance
  ( AsUnion as r
  , HasAcceptCheck cs
  , ReflectMethod method
  , ResponseListRender cs as
  )
  => HasServer (MultiVerb method cs as r) ctx
  where
  type ServerT (MultiVerb method cs as r) m = m r

  hoistServerWithContext _ _ f = f

  route
    :: forall env
     . Proxy (MultiVerb method cs as r)
    -> Context ctx
    -> Delayed env (Handler r)
    -> Router env
  route _ _ action = leafRouter $ \env req k -> do
    let acc = getAcceptHeader req
        action' =
          action
            `addMethodCheck` methodCheck method req
            `addAcceptCheck` acceptCheck' (Proxy @cs) acc
    runAction action' env req k $ \output -> do
      let mresp = responseListRender @cs @as acc (toUnion @as output)
      someResponseToWai <$> case mresp of
        Nothing -> FailFatal err406
        Just resp
          | allowedMethodHead method req -> pure (setEmptyBody resp)
          | otherwise -> pure resp
    where
      method = reflectMethod (Proxy @method)

class HasAcceptCheck cs where
  acceptCheck' :: Proxy cs -> AcceptHeader -> DelayedIO ()

instance AllMime cs => HasAcceptCheck cs where
  acceptCheck' = acceptCheck

instance HasAcceptCheck '() where
  acceptCheck' _ _ = pure ()
