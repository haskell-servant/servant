{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Servant.API.Get where

import Control.Monad.Trans.Either
import Data.Aeson
import Data.Proxy
import Data.String.Conversions
import Data.Typeable
import Network.HTTP.Types
import Network.Wai
import Servant.Client
import Servant.Docs
import Servant.Server
import Servant.Utils.Client

-- | Endpoint for simple GET requests. The server doesn't receive any arguments
-- and serves the contained type as JSON.
data Get a
  deriving Typeable

instance ToJSON result => HasServer (Get result) where
  type Server (Get result) = EitherT (Int, String) IO result
  route Proxy action request respond
    | null (pathInfo request) && requestMethod request == methodGet = do
        e <- runEitherT action
        respond . succeedWith $ case e of
          Right output ->
            responseLBS ok200 [("Content-Type", "application/json")] (encode output)
          Left (status, message) ->
            responseLBS (mkStatus status (cs message)) [] (cs message)
    | null (pathInfo request) && requestMethod request /= methodGet =
        respond $ failWith WrongMethod
    | otherwise = respond $ failWith NotFound

instance FromJSON result => HasClient (Get result) where
  type Client (Get result) = BaseUrl -> EitherT String IO result
  clientWithRoute Proxy req host =
    performRequest methodGet req 200 host

instance ToSample a => HasDocs (Get a) where
  docsFor Proxy (endpoint, action) =
    single endpoint' action'

    where endpoint' = endpoint & method .~ DocGET
          action' = action & response.respBody .~ toSample p
          p = Proxy :: Proxy a
