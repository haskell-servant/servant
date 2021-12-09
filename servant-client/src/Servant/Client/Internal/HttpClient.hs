{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
module Servant.Client.Internal.HttpClient where

import           Prelude ()
import           Prelude.Compat

import           Control.Concurrent.MVar
                 (modifyMVar, newMVar)
import           Control.Concurrent.STM.TVar
import           Control.Exception
                 (SomeException (..), catch)
import           Control.Monad
                 (unless)
import           Control.Monad.Base
                 (MonadBase (..))
import           Control.Monad.Catch
                 (MonadCatch, MonadThrow)
import           Control.Monad.Error.Class
                 (MonadError (..))
import           Control.Monad.IO.Class
                 (MonadIO (..))
import           Control.Monad.Reader
                 (MonadReader, ReaderT, ask, runReaderT)
import           Control.Monad.STM
                 (STM, atomically)
import           Control.Monad.Trans.Control
                 (MonadBaseControl (..))
import           Control.Monad.Trans.Except
                 (ExceptT, runExceptT)
import           Data.Bifunctor
                 (bimap)
import qualified Data.ByteString             as BS
import           Data.ByteString.Builder
                 (toLazyByteString)
import qualified Data.ByteString.Lazy        as BSL
import           Data.Either
                 (either)
import           Data.Foldable
                 (foldl',toList)
import           Data.Functor.Alt
                 (Alt (..))
import           Data.Maybe
                 (maybe, maybeToList)
import           Data.Proxy
                 (Proxy (..))
import           Data.Sequence
                 (fromList)
import           Data.String
                 (fromString)
import           Data.Time.Clock
                 (UTCTime, getCurrentTime)
import           GHC.Generics
import           Network.HTTP.Media
                 (renderHeader)
import           Network.HTTP.Types
                 (hContentType, renderQuery, statusIsSuccessful, urlEncode, Status)
import           Servant.Client.Core

import qualified Network.HTTP.Client         as Client
import qualified Servant.Types.SourceT       as S

-- | The environment in which a request is run.
--   The 'baseUrl' and 'makeClientRequest' function are used to create a @http-client@ request.
--   Cookies are then added to that request if a 'CookieJar' is set on the environment.
--   Finally the request is executed with the 'manager'.
--   The 'makeClientRequest' function can be used to modify the request to execute and set values which
--   are not specified on a @servant@ 'Request' like 'responseTimeout' or 'redirectCount'
data ClientEnv
  = ClientEnv
  { manager :: Client.Manager
  , baseUrl :: BaseUrl
  , cookieJar :: Maybe (TVar Client.CookieJar)
  , makeClientRequest :: BaseUrl -> Request -> Client.Request
  -- ^ this function can be used to customize the creation of @http-client@ requests from @servant@ requests. Default value: 'defaultMakeClientRequest'
  --   Note that:
  --      1. 'makeClientRequest' exists to allow overriding operational semantics e.g. 'responseTimeout' per request,
  --          If you need global modifications, you should use 'managerModifyRequest'
  --      2. the 'cookieJar', if defined, is being applied after 'makeClientRequest' is called.
  }

-- | 'ClientEnv' smart constructor.
mkClientEnv :: Client.Manager -> BaseUrl -> ClientEnv
mkClientEnv mgr burl = ClientEnv mgr burl Nothing defaultMakeClientRequest

-- | Generates a set of client functions for an API.
--
-- Example:
--
-- > type API = Capture "no" Int :> Get '[JSON] Int
-- >        :<|> Get '[JSON] [Bool]
-- >
-- > api :: Proxy API
-- > api = Proxy
-- >
-- > getInt :: Int -> ClientM Int
-- > getBools :: ClientM [Bool]
-- > getInt :<|> getBools = client api
client :: HasClient ClientM api => Proxy api -> Client ClientM api
client api = api `clientIn` (Proxy :: Proxy ClientM)

-- | Change the monad the client functions live in, by
--   supplying a conversion function
--   (a natural transformation to be precise).
--
--   For example, assuming you have some @manager :: 'Manager'@ and
--   @baseurl :: 'BaseUrl'@ around:
--
--   > type API = Get '[JSON] Int :<|> Capture "n" Int :> Post '[JSON] Int
--   > api :: Proxy API
--   > api = Proxy
--   > getInt :: IO Int
--   > postInt :: Int -> IO Int
--   > getInt :<|> postInt = hoistClient api (flip runClientM cenv) (client api)
--   >   where cenv = mkClientEnv manager baseurl
hoistClient
  :: HasClient ClientM api
  => Proxy api
  -> (forall a. m a -> n a)
  -> Client m api
  -> Client n api
hoistClient = hoistClientMonad (Proxy :: Proxy ClientM)

-- | @ClientM@ is the monad in which client functions run. Contains the
-- 'Client.Manager' and 'BaseUrl' used for requests in the reader environment.
newtype ClientM a = ClientM
  { unClientM :: ReaderT ClientEnv (ExceptT ClientError IO) a }
  deriving ( Functor, Applicative, Monad, MonadIO, Generic
           , MonadReader ClientEnv, MonadError ClientError, MonadThrow
           , MonadCatch)

instance MonadBase IO ClientM where
  liftBase = ClientM . liftBase

instance MonadBaseControl IO ClientM where
  type StM ClientM a = Either ClientError a

  liftBaseWith f = ClientM (liftBaseWith (\g -> f (g . unClientM)))

  restoreM st = ClientM (restoreM st)

-- | Try clients in order, last error is preserved.
instance Alt ClientM where
  a <!> b = a `catchError` \_ -> b

instance RunClient ClientM where
  runRequestAcceptStatus = performRequest
  throwClientError = throwError

runClientM :: ClientM a -> ClientEnv -> IO (Either ClientError a)
runClientM cm env = runExceptT $ flip runReaderT env $ unClientM cm

performRequest :: Maybe [Status] -> Request -> ClientM Response
performRequest acceptStatus req = do
  ClientEnv m burl cookieJar' createClientRequest <- ask
  let clientRequest = createClientRequest burl req
  request <- case cookieJar' of
    Nothing -> pure clientRequest
    Just cj -> liftIO $ do
      now <- getCurrentTime
      atomically $ do
        oldCookieJar <- readTVar cj
        let (newRequest, newCookieJar) =
              Client.insertCookiesIntoRequest
                clientRequest
                oldCookieJar
                now
        writeTVar cj newCookieJar
        pure newRequest

  response <- maybe (requestWithoutCookieJar m request) (requestWithCookieJar m request) cookieJar'
  let status = Client.responseStatus response
      ourResponse = clientResponseToResponse id response
      goodStatus = case acceptStatus of
        Nothing -> statusIsSuccessful status
        Just good -> status `elem` good
  unless goodStatus $ do
    throwError $ mkFailureResponse burl req ourResponse
  return ourResponse
  where
    requestWithoutCookieJar :: Client.Manager -> Client.Request -> ClientM (Client.Response BSL.ByteString)
    requestWithoutCookieJar m' request' = do
        eResponse <- liftIO . catchConnectionError $ Client.httpLbs request' m'
        either throwError return eResponse

    requestWithCookieJar :: Client.Manager -> Client.Request -> TVar Client.CookieJar -> ClientM (Client.Response BSL.ByteString)
    requestWithCookieJar m' request' cj = do
        eResponse <- liftIO . catchConnectionError . Client.withResponseHistory request' m' $ updateWithResponseCookies cj
        either throwError return eResponse

    updateWithResponseCookies :: TVar Client.CookieJar -> Client.HistoriedResponse Client.BodyReader -> IO (Client.Response BSL.ByteString)
    updateWithResponseCookies cj responses = do
        now <- getCurrentTime
        bss <- Client.brConsume $ Client.responseBody fRes
        let fRes'        = fRes { Client.responseBody = BSL.fromChunks bss }
            allResponses = Client.hrRedirects responses <> [(fReq, fRes')]
        atomically $ mapM_ (updateCookieJar now) allResponses
        return fRes'
      where
          updateCookieJar :: UTCTime -> (Client.Request, Client.Response BSL.ByteString) -> STM ()
          updateCookieJar now' (req', res') = modifyTVar' cj (fst . Client.updateCookieJar res' req' now')

          fReq = Client.hrFinalRequest responses
          fRes = Client.hrFinalResponse responses

mkFailureResponse :: BaseUrl -> Request -> ResponseF BSL.ByteString -> ClientError
mkFailureResponse burl request =
    FailureResponse (bimap (const ()) f request)
  where
    f b = (burl, BSL.toStrict $ toLazyByteString b)

clientResponseToResponse :: (a -> b) -> Client.Response a -> ResponseF b
clientResponseToResponse f r = Response
    { responseStatusCode  = Client.responseStatus r
    , responseBody        = f (Client.responseBody r)
    , responseHeaders     = fromList $ Client.responseHeaders r
    , responseHttpVersion = Client.responseVersion r
    }

-- | Create a @http-client@ 'Client.Request' from a @servant@ 'Request'
--    The 'Client.host', 'Client.path' and 'Client.port' fields are extracted from the 'BaseUrl'
--    otherwise the body, headers and query string are derived from the @servant@ 'Request'
defaultMakeClientRequest :: BaseUrl -> Request -> Client.Request
defaultMakeClientRequest burl r = Client.defaultRequest
    { Client.method = requestMethod r
    , Client.host = fromString $ baseUrlHost burl
    , Client.port = baseUrlPort burl
    , Client.path = BSL.toStrict
                  $ fromString (baseUrlPath burl)
                 <> toLazyByteString (requestPath r)
    , Client.queryString = buildQueryString . toList $ requestQueryString r
    , Client.requestHeaders =
      maybeToList acceptHdr ++ maybeToList contentTypeHdr ++ headers
    , Client.requestBody = body
    , Client.secure = isSecure
    }
  where
    -- Content-Type and Accept are specified by requestBody and requestAccept
    headers = filter (\(h, _) -> h /= "Accept" && h /= "Content-Type") $
        toList $requestHeaders r

    acceptHdr
        | null hs   = Nothing
        | otherwise = Just ("Accept", renderHeader hs)
      where
        hs = toList $ requestAccept r

    convertBody bd = case bd of
        RequestBodyLBS body'       -> Client.RequestBodyLBS body'
        RequestBodyBS body'        -> Client.RequestBodyBS body'
        RequestBodySource sourceIO -> Client.RequestBodyStreamChunked givesPopper
          where
            givesPopper :: (IO BS.ByteString -> IO ()) -> IO ()
            givesPopper needsPopper = S.unSourceT sourceIO $ \step0 -> do
                ref <- newMVar step0

                -- Note sure we need locking, but it's feels safer.
                let popper :: IO BS.ByteString
                    popper = modifyMVar ref nextBs

                needsPopper popper

            nextBs S.Stop          = return (S.Stop, BS.empty)
            nextBs (S.Error err)   = fail err
            nextBs (S.Skip s)      = nextBs s
            nextBs (S.Effect ms)   = ms >>= nextBs
            nextBs (S.Yield lbs s) = case BSL.toChunks lbs of
                []     -> nextBs s
                (x:xs) | BS.null x -> nextBs step'
                       | otherwise -> return (step', x)
                    where
                      step' = S.Yield (BSL.fromChunks xs) s

    (body, contentTypeHdr) = case requestBody r of
        Nothing           -> (Client.RequestBodyBS "", Nothing)
        Just (body', typ) -> (convertBody body', Just (hContentType, renderHeader typ))

    isSecure = case baseUrlScheme burl of
        Http -> False
        Https -> True

    -- Query string builder which does not do any encoding
    buildQueryString = ("?" <>) . foldl' addQueryParam mempty

    addQueryParam qs (k, v) =
          qs <> (if BS.null qs then mempty else "&") <> urlEncode True k <> foldMap ("=" <>) v


catchConnectionError :: IO a -> IO (Either ClientError a)
catchConnectionError action =
  catch (Right <$> action) $ \e ->
    pure . Left . ConnectionError $ SomeException (e :: Client.HttpException)
