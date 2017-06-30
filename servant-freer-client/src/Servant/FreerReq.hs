{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}

module Servant.FreerReq where

import           Control.Exception (toException)
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception
import           Control.Monad.Freer.Http
import           Control.Monad.Freer.Reader
import           Data.ByteString.Lazy hiding (pack, filter, map, null, elem, any)
import           Data.String.Conversions (cs)
import           Network.HTTP.Client (Response)
import qualified Network.HTTP.Client as Client
import           Network.HTTP.Media
import           Network.HTTP.Types
import qualified Network.HTTP.Types.Header   as HTTP
import           Servant.Common.Req hiding (ClientM, runClientM')

newtype ClientM r a = ClientM { runClientM' :: ( Member (Reader ClientEnv) r
                                               , Member (Exc ServantError) r
                                               , Member Http r) => (Eff r) a }

instance Functor (ClientM r) where
  fmap f a = ClientM $ fmap f (runClientM' a)

instance Applicative (ClientM r) where
  pure a = ClientM $ pure a
  f <*> a = ClientM $ (runClientM' f) <*> (runClientM' a)

instance Monad (ClientM r) where
  a >>= f = ClientM $ (runClientM' a) >>= (runClientM' . f)

performRequest :: ( Member Http r
                  , Member (Reader ClientEnv) r
                  , Member (Exc ServantError) r)
               => Method -> Req -> ClientM r ( Int, ByteString, MediaType
                                             , [HTTP.Header], Response ByteString)
performRequest reqMethod req = ClientM $ do
  m <- asks manager
  reqHost <- asks baseUrl
  response <- case (reqToRequest req reqHost) of
    Left some -> throwError . ConnectionError $ toException some
    Right request -> doRequest request
  let status = Client.responseStatus response
      body = Client.responseBody response
      hdrs = Client.responseHeaders response
      status_code = statusCode status
  ct <- case lookup "Content-Type" $ Client.responseHeaders response of
    Nothing -> pure $ "application"//"octet-stream"
    Just t -> case parseAccept t of
      Nothing -> throwError $ InvalidContentTypeHeader (cs t) body
      Just t' -> pure t'
  return (status_code, body, ct, hdrs, response)

runClientM :: ClientM ('[ Reader ClientEnv
                        , Exc ServantError
                        , Http
                        , IO]) a -> ClientEnv -> IO (Either ServantError a)
runClientM cm ce@ClientEnv { manager = mgr } =
  runM . (runHttp mgr) . runError . (flip runReader ce) $ runClientM' cm
