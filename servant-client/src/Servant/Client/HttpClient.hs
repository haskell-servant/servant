{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

{-| http-client based client  requests executor -}
module Servant.Client.HttpClient where


import           Prelude                     ()
import           Prelude.Compat

import           Control.Exception
import           Control.Monad
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Catch         (MonadCatch, MonadThrow)
import           Control.Monad.Error.Class   (MonadError (..))
import           Control.Monad.IO.Class      ()
import           Control.Monad.Reader
import           Control.Monad.Trans.Control (MonadBaseControl (..))
import           Control.Monad.Trans.Except
import           Data.ByteString.Lazy        hiding (any, elem, filter, map,
                                              null, pack)
import           Data.Foldable               (toList)
import           Data.Functor.Alt            (Alt (..))
import           Data.Proxy
import           Data.String.Conversions     (cs)
import           GHC.Generics
import           Network.HTTP.Media
import           Network.HTTP.Types
import           Servant.API.ContentTypes
import           Servant.Client.Class
import           Servant.Common.BaseUrl
import           Servant.Common.Req

import qualified Network.HTTP.Client         as Client
import qualified Network.HTTP.Types.Header   as HTTP

instance RunClient ClientM NoContent ( Int, ByteString, MediaType
                                     , [HTTP.Header], Response ByteString) where
  runRequest _ meth req = performRequest meth req

instance (MimeUnrender ct a) =>
         RunClient ClientM ct ([HTTP.Header], a) where
  runRequest p meth req = performRequestCT p meth req

instance RunClient ClientM NoContent [HTTP.Header] where
  runRequest _ meth req = performRequestNoBody meth req

data ClientEnv
  = ClientEnv
  { manager :: Manager
  , baseUrl :: BaseUrl
  }


-- | @ClientM@ is the monad in which client functions run. Contains the
-- 'Manager' and 'BaseUrl' used for requests in the reader environment.
newtype ClientM a = ClientM { runClientM' :: ReaderT ClientEnv (ExceptT ServantError IO) a }
                    deriving ( Functor, Applicative, Monad, MonadIO, Generic
                             , MonadReader ClientEnv
                             , MonadError ServantError
                             , MonadThrow, MonadCatch
                             )

instance MonadBase IO ClientM where
  liftBase = ClientM . liftBase

instance MonadBaseControl IO ClientM where
  type StM ClientM a = Either ServantError a

  liftBaseWith f = ClientM (liftBaseWith (\g -> f (g . runClientM')))

  restoreM st = ClientM (restoreM st)

-- | Try clients in order, last error is preserved.
instance Alt ClientM where
  a <!> b = a `catchError` \_ -> b

runClientM :: ClientM a -> ClientEnv -> IO (Either ServantError a)
runClientM cm env = runExceptT $ (flip runReaderT env) $ runClientM' cm


performRequest :: Method -> Req
               -> ClientM Response
performRequest reqMethod req = do
  m <- asks manager
  reqHost <- asks baseUrl
  partialRequest <- liftIO $ reqToRequest req reqHost

  let request = partialRequest { Client.method = reqMethod }

  eResponse <- liftIO $ catchConnectionError $ Client.httpLbs request m
  case eResponse of
    Left err ->
      throwError . ConnectionError $ SomeException err

    Right response -> do
      let status = Client.responseStatus response
          body = Client.responseBody response
          hdrs = Client.responseHeaders response
          status_code = statusCode status
      ct <- case lookup "Content-Type" $ Client.responseHeaders response of
                 Nothing -> pure $ "application"//"octet-stream"
                 Just t -> case parseAccept t of
                   Nothing -> throwError $ InvalidContentTypeHeader (cs t) body
                   Just t' -> pure t'
      unless (status_code >= 200 && status_code < 300) $
        throwError $ FailureResponse (UrlReq reqHost req) status ct body
      return (status_code, body, ct, hdrs, response)

performRequestCT :: MimeUnrender ct result => Proxy ct -> Method -> Req
    -> ClientM ([HTTP.Header], result)
performRequestCT ct reqMethod req = do
  let acceptCTS = contentTypes ct
  (_status, respBody, respCT, hdrs, _response) <-
    performRequest reqMethod (req { reqAccept = toList acceptCTS })
  unless (any (matches respCT) acceptCTS) $ throwError $ UnsupportedContentType respCT respBody
  case mimeUnrender ct respBody of
    Left err -> throwError $ DecodeFailure err respCT respBody
    Right val -> return (hdrs, val)

performRequestNoBody :: Method -> Req -> ClientM [HTTP.Header]
performRequestNoBody reqMethod req = do
  (_status, _body, _ct, hdrs, _response) <- performRequest reqMethod req
  return hdrs

catchConnectionError :: IO a -> IO (Either ServantError a)
catchConnectionError action =
  catch (Right <$> action) $ \e ->
    pure . Left . ConnectionError $ SomeException (e :: HttpException)
