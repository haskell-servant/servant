{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
module Servant.Client.Internal.HttpClient where

import           Prelude                     ()
import           Prelude.Compat

import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Catch         (MonadCatch, MonadThrow)
import           Control.Monad.Error.Class   (MonadError (..))
import           Control.Monad.Reader
import           Control.Monad.STM           (atomically)
import           Control.Monad.Trans.Control (MonadBaseControl (..))
import           Control.Monad.Trans.Except
import           Data.ByteString.Builder     (toLazyByteString)
import qualified Data.ByteString.Lazy        as BSL
import           Data.Foldable               (toList, for_)
import           Data.Functor.Alt            (Alt (..))
import           Data.Maybe                  (maybeToList)
import           Data.Semigroup              ((<>))
import           Data.Proxy                  (Proxy (..))
import           Data.Sequence               (fromList)
import           Data.String                 (fromString)
import qualified Data.Text                   as T
import           Data.Time.Clock             (getCurrentTime)
import           GHC.Generics
import           Network.HTTP.Media          (renderHeader)
import           Network.HTTP.Types          (hContentType, renderQuery,
                                              statusCode)
import           Servant.Client.Core

import qualified Network.HTTP.Client         as Client

-- | The environment in which a request is run.
data ClientEnv
  = ClientEnv
  { manager :: Client.Manager
  , baseUrl :: BaseUrl
  , cookieJar :: Maybe (TVar Client.CookieJar)
  }

-- | 'ClientEnv' smart constructor.
mkClientEnv :: Client.Manager -> BaseUrl -> ClientEnv
mkClientEnv mgr burl = ClientEnv mgr burl Nothing

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

-- | @ClientM@ is the monad in which client functions run. Contains the
-- 'Client.Manager' and 'BaseUrl' used for requests in the reader environment.
newtype ClientM a = ClientM
  { unClientM :: ReaderT ClientEnv (ExceptT ServantError IO) a }
  deriving ( Functor, Applicative, Monad, MonadIO, Generic
           , MonadReader ClientEnv, MonadError ServantError, MonadThrow
           , MonadCatch)

instance MonadBase IO ClientM where
  liftBase = ClientM . liftBase

instance MonadBaseControl IO ClientM where
  type StM ClientM a = Either ServantError a

  liftBaseWith f = ClientM (liftBaseWith (\g -> f (g . unClientM)))

  restoreM st = ClientM (restoreM st)

-- | Try clients in order, last error is preserved.
instance Alt ClientM where
  a <!> b = a `catchError` \_ -> b

instance RunClient ClientM where
  runRequest = performRequest
  streamingRequest = performStreamingRequest
  throwServantError = throwError
  catchServantError = catchError

instance ClientLike (ClientM a) (ClientM a) where
  mkClient = id

runClientM :: ClientM a -> ClientEnv -> IO (Either ServantError a)
runClientM cm env = runExceptT $ flip runReaderT env $ unClientM cm

performRequest :: Request -> ClientM Response
performRequest req = do
  ClientEnv m burl cookieJar' <- ask
  let clientRequest = requestToClientRequest burl req
  request <- case cookieJar' of
    Nothing -> pure clientRequest
    Just cj -> liftIO $ do
      now <- getCurrentTime
      atomically $ do
        oldCookieJar <- readTVar cj
        let (newRequest, newCookieJar) =
              Client.insertCookiesIntoRequest
                (requestToClientRequest burl req)
                oldCookieJar
                now
        writeTVar cj newCookieJar
        pure newRequest

  eResponse <- liftIO $ catchConnectionError $ Client.httpLbs request m
  case eResponse of
    Left err -> throwError err
    Right response -> do
      for_ cookieJar' $ \cj -> liftIO $ do
        now' <- getCurrentTime
        atomically $ modifyTVar' cj (fst . Client.updateCookieJar response request now')
      let status = Client.responseStatus response
          status_code = statusCode status
          ourResponse = clientResponseToResponse response
      unless (status_code >= 200 && status_code < 300) $
        throwError $ FailureResponse ourResponse
      return ourResponse

performStreamingRequest :: Request -> ClientM StreamingResponse
performStreamingRequest req = do
  m <- asks manager
  burl <- asks baseUrl
  let request = requestToClientRequest burl req
  return $ StreamingResponse $
    \k -> Client.withResponse request m $
    \r -> do
      let status = Client.responseStatus r
          status_code = statusCode status
      unless (status_code >= 200 && status_code < 300) $ do
        b <- BSL.fromChunks <$> Client.brConsume (Client.responseBody r)
        throw $ FailureResponse $ clientResponseToResponse r { Client.responseBody = b }
      k (clientResponseToResponse r)

clientResponseToResponse :: Client.Response a -> GenResponse a
clientResponseToResponse r = Response
  { responseStatusCode = Client.responseStatus r
  , responseBody = Client.responseBody r
  , responseHeaders = fromList $ Client.responseHeaders r
  , responseHttpVersion = Client.responseVersion r
  }

requestToClientRequest :: BaseUrl -> Request -> Client.Request
requestToClientRequest burl r = Client.defaultRequest
  { Client.method = requestMethod r
  , Client.host = fromString $ baseUrlHost burl
  , Client.port = baseUrlPort burl
  , Client.path = BSL.toStrict
                $ fromString (baseUrlPath burl)
               <> toLazyByteString (requestPath r)
  , Client.queryString = renderQuery True . toList $ requestQueryString r
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

    (body, contentTypeHdr) = case requestBody r of
      Nothing -> (Client.RequestBodyLBS "", Nothing)
      Just (RequestBodyLBS body', typ)
        -> (Client.RequestBodyLBS body', Just (hContentType, renderHeader typ))

    isSecure = case baseUrlScheme burl of
      Http -> False
      Https -> True

catchConnectionError :: IO a -> IO (Either ServantError a)
catchConnectionError action =
  catch (Right <$> action) $ \e ->
    pure . Left . ConnectionError . T.pack $ show (e :: Client.HttpException)
