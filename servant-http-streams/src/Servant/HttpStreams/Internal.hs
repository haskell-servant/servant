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
module Servant.HttpStreams.Internal where

import           Prelude ()
import           Prelude.Compat

import           Control.DeepSeq
                 (NFData, force)
import           Control.Exception
                 (IOException, SomeException (..), catch, evaluate, throwIO)
import           Control.Monad
                 (unless)
import           Control.Monad.Base
                 (MonadBase (..))
import           Control.Monad.Codensity
                 (Codensity (..))
import           Control.Monad.Error.Class
                 (MonadError (..))
import           Control.Monad.IO.Class
                 (MonadIO (..))
import           Control.Monad.Reader
                 (MonadReader, ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Class
                 (lift)
import           Control.Monad.Trans.Except
                 (ExceptT, runExceptT)
import           Data.Bifunctor
                 (bimap, first)
import           Data.ByteString.Builder
                 (toLazyByteString)
import qualified Data.ByteString.Builder    as B
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.CaseInsensitive       as CI
import           Data.Foldable
                 (for_, toList)
import           Data.Functor.Alt
                 (Alt (..))
import           Data.Maybe
                 (maybeToList)
import           Data.Proxy
                 (Proxy (..))
import           Data.Sequence
                 (fromList)
import           Data.String
                 (fromString)
import           GHC.Generics
import           Network.HTTP.Media
                 (renderHeader)
import           Network.HTTP.Types
                 (Status (..), hContentType, http11, renderQuery, statusIsSuccessful)
import           Servant.Client.Core

import qualified Network.Http.Client        as Client
import qualified Network.Http.Types         as Client
import qualified Servant.Types.SourceT      as S
import qualified System.IO.Streams          as Streams

-- | The environment in which a request is run.
--
-- 'ClientEnv' carries an open connection. See 'withClientEnvIO'.
--
data ClientEnv
    = ClientEnv
    { baseUrl    :: BaseUrl
    , connection :: Client.Connection
    }

-- | 'ClientEnv' smart constructor.
mkClientEnv :: BaseUrl -> Client.Connection -> ClientEnv
mkClientEnv = ClientEnv

-- | Open a connection to 'BaseUrl'.
withClientEnvIO :: BaseUrl -> (ClientEnv -> IO r) -> IO r
withClientEnvIO burl k = Client.withConnection open $ \conn ->
    k (mkClientEnv burl conn)
  where
    open = Client.openConnection (fromString $ baseUrlHost burl) (fromIntegral $ baseUrlPort burl)

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

runClientM :: NFData a => ClientM a -> ClientEnv -> IO (Either ClientError a)
runClientM cm env = withClientM cm env (evaluate . force)

withClientM :: ClientM a -> ClientEnv -> (Either ClientError a -> IO b) -> IO b
withClientM cm env k =
    let Codensity f = runExceptT $ flip runReaderT env $ unClientM cm
    in f k

performRequest :: Maybe [Status] -> Request -> ClientM Response
performRequest acceptStatus req = do
    ClientEnv burl conn <- ask
    let (req', body) = requestToClientRequest burl req
    x <- ClientM $ lift $ lift $ Codensity $ \k -> do
        Client.sendRequest conn req' body
        Client.receiveResponse conn $ \res' body' -> do
            let status = toEnum $ Client.getStatusCode res'
            lbs <- BSL.fromChunks <$> Streams.toList body'
            let res'' = clientResponseToResponse res' lbs
                goodStatus = case acceptStatus of
                  Nothing -> statusIsSuccessful status
                  Just good -> status `elem` good
            if goodStatus
            then k (Right res'')
            else k (Left (mkFailureResponse burl req res''))

    either throwError pure x

performWithStreamingRequest :: Request -> (StreamingResponse -> IO a) -> ClientM a
performWithStreamingRequest req k = do
    ClientEnv burl conn <- ask
    let (req', body) = requestToClientRequest burl req
    ClientM $ lift $ lift $ Codensity $ \k1 -> do
        Client.sendRequest conn req' body
        Client.receiveResponseRaw conn $ \res' body' -> do
            -- check status code
            let status = toEnum $ Client.getStatusCode res'
            unless (statusIsSuccessful status) $ do
                lbs <- BSL.fromChunks <$> Streams.toList body'
                throwIO $ mkFailureResponse burl req (clientResponseToResponse res' lbs)

            x <- k (clientResponseToResponse res' (fromInputStream body'))
            k1 x

mkFailureResponse :: BaseUrl -> Request -> ResponseF BSL.ByteString -> ClientError
mkFailureResponse burl request =
    FailureResponse (bimap (const ()) f request)
  where
    f b = (burl, BSL.toStrict $ toLazyByteString b)

clientResponseToResponse :: Client.Response -> body -> ResponseF body
clientResponseToResponse r body = Response
    { responseStatusCode  = Status (Client.getStatusCode r) (Client.getStatusMessage r)
    , responseBody        = body
    , responseHeaders     = fromList $ map (first CI.mk) $ Client.retrieveHeaders $ Client.getHeaders r
    , responseHttpVersion = http11 -- guess
    }

requestToClientRequest :: BaseUrl -> Request -> (Client.Request, Streams.OutputStream B.Builder -> IO ())
requestToClientRequest burl r = (request, body)
  where
    request = Client.buildRequest1 $ do
        Client.http (Client.Method $ requestMethod r)
            $ fromString (baseUrlPath burl)
            <> BSL.toStrict (toLazyByteString (requestPath r))
            <> renderQuery True (toList (requestQueryString r))
        -- We are connected, but we still need to know what we try to query
        Client.setHostname (fromString $ baseUrlHost burl) (fromIntegral $ baseUrlPort burl)
        for_ (maybeToList acceptHdr ++ maybeToList contentTypeHdr ++ headers) $ \(hn, hv) ->
            Client.setHeader (CI.original hn) hv

        -- body is always chunked
        Client.setTransferEncoding

    -- Content-Type and Accept are specified by requestBody and requestAccept
    headers = filter (\(h, _) -> h /= "Accept" && h /= "Content-Type") $
        toList $ requestHeaders r

    acceptHdr
        | null hs   = Nothing
        | otherwise = Just ("Accept", renderHeader hs)
      where
        hs = toList $ requestAccept r

    convertBody bd os = case bd of
        RequestBodyLBS body' ->
            Streams.writeTo os (Just (B.lazyByteString body'))
        RequestBodyBS body' ->
            Streams.writeTo os (Just (B.byteString body'))
        RequestBodySource sourceIO ->
            toOutputStream sourceIO os

    (body, contentTypeHdr) = case requestBody r of
        Nothing           -> (Client.emptyBody, Nothing)
        Just (body', typ) -> (convertBody body', Just (hContentType, renderHeader typ))

catchConnectionError :: IO a -> IO (Either ClientError a)
catchConnectionError action =
  catch (Right <$> action) $ \e ->
    pure . Left . ConnectionError $ SomeException (e :: IOException)

fromInputStream :: Streams.InputStream b -> S.SourceT IO b
fromInputStream is = S.SourceT $ \k -> k loop where
    loop = S.Effect $ maybe S.Stop (flip S.Yield loop) <$> Streams.read is

toOutputStream :: S.SourceT IO BSL.ByteString -> Streams.OutputStream B.Builder -> IO ()
toOutputStream (S.SourceT k) os = k loop where
    loop S.Stop        = return ()
    loop (S.Error err) = fail err
    loop (S.Skip s)    = loop s
    loop (S.Effect mx) = mx >>= loop
    loop (S.Yield x s) = Streams.write (Just (B.lazyByteString x)) os >> loop s
