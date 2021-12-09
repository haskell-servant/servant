{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
module Servant.Client.Internal.HttpClient.Streaming (
    module Servant.Client.Internal.HttpClient.Streaming,
    ClientEnv (..),
    mkClientEnv,
    clientResponseToResponse,
    defaultMakeClientRequest,
    catchConnectionError,
    ) where

import           Prelude ()
import           Prelude.Compat

import           Control.Concurrent.STM.TVar
import           Control.DeepSeq
                 (NFData, force)
import           Control.Exception
                 (evaluate, throwIO)
import           Control.Monad ()
import           Control.Monad.Base
                 (MonadBase (..))
import           Control.Monad.Codensity
                 (Codensity (..))
import           Control.Monad.Error.Class
                 (MonadError (..))
import           Control.Monad.Reader
import           Control.Monad.STM
                 (atomically)
import           Control.Monad.Trans.Except
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Lazy               as BSL
import           Data.Foldable
                 (for_)
import           Data.Functor.Alt
                 (Alt (..))
import           Data.Proxy
                 (Proxy (..))
import           Data.Time.Clock
                 (getCurrentTime)
import           GHC.Generics
import           Network.HTTP.Types
                 (Status, statusIsSuccessful)

import qualified Network.HTTP.Client                as Client

import           Servant.Client.Core
import           Servant.Client.Internal.HttpClient
                 (ClientEnv (..), catchConnectionError,
                 clientResponseToResponse, mkClientEnv, mkFailureResponse,
                 defaultMakeClientRequest)
import qualified Servant.Types.SourceT              as S


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
  { unClientM :: ReaderT ClientEnv (ExceptT ClientError (Codensity IO)) a }
  deriving ( Functor, Applicative, Monad, MonadIO, Generic
           , MonadReader ClientEnv, MonadError ClientError)

instance MonadBase IO ClientM where
  liftBase = ClientM . liftIO

-- | Try clients in order, last error is preserved.
instance Alt ClientM where
  a <!> b = a `catchError` \_ -> b

instance RunClient ClientM where
  runRequestAcceptStatus = performRequest
  throwClientError = throwError

instance RunStreamingClient ClientM where
  withStreamingRequest = performWithStreamingRequest

withClientM :: ClientM a -> ClientEnv -> (Either ClientError a -> IO b) -> IO b
withClientM cm env k =
    let Codensity f = runExceptT $ flip runReaderT env $ unClientM cm
    in f k

-- | A 'runClientM' variant for streaming client.
--
-- It allows using this module's 'ClientM' in a direct style.
-- The 'NFData' constraint however prevents using this function with genuine
-- streaming response types ('SourceT', 'Conduit', pipes 'Proxy' or 'Machine').
-- For those you have to use 'withClientM'.
--
-- /Note:/ we 'force' the result, so the likelihood of accidentally leaking a
-- connection is smaller. Use with care.
--
runClientM :: NFData a => ClientM a -> ClientEnv -> IO (Either ClientError a)
runClientM cm env = withClientM cm env (evaluate . force)

performRequest :: Maybe [Status] -> Request -> ClientM Response
performRequest acceptStatus req = do
    -- TODO: should use Client.withResponse here too
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

  eResponse <- liftIO $ catchConnectionError $ Client.httpLbs request m
  case eResponse of
    Left err -> throwError err
    Right response -> do
      for_ cookieJar' $ \cj -> liftIO $ do
        now' <- getCurrentTime
        atomically $ modifyTVar' cj (fst . Client.updateCookieJar response request now')
      let status = Client.responseStatus response
          ourResponse = clientResponseToResponse id response
          goodStatus = case acceptStatus of
            Nothing -> statusIsSuccessful status
            Just good -> status `elem` good
      unless goodStatus $ do
        throwError $ mkFailureResponse burl req ourResponse
      return ourResponse

-- | TODO: support UVerb ('acceptStatus' argument, like in 'performRequest' above).
performWithStreamingRequest :: Request -> (StreamingResponse -> IO a) -> ClientM a
performWithStreamingRequest req k = do
  m <- asks manager
  burl <- asks baseUrl
  createClientRequest <- asks makeClientRequest
  let request = createClientRequest burl req
  ClientM $ lift $ lift $ Codensity $ \k1 ->
      Client.withResponse request m $ \res -> do
          let status = Client.responseStatus res

          -- we throw FailureResponse in IO :(
          unless (statusIsSuccessful status) $ do
              b <- BSL.fromChunks <$> Client.brConsume (Client.responseBody res)
              throwIO $ mkFailureResponse burl req (clientResponseToResponse (const b) res)

          x <- k (clientResponseToResponse (S.fromAction BS.null) res)
          k1 x
