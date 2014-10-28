{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.API.Get where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Proxy
import Data.String.Conversions
import Network.HTTP.Types
import Network.URI
import Network.Wai
import Servant.Client
import Servant.Docs
import Servant.Server

import qualified Network.HTTP.Client as Client

-- | Endpoint for simple GET requests. The server doesn't receive any arguments
-- and serves the contained type as JSON.
data Get a

instance ToJSON result => HasServer (Get result) where
  type Server (Get result) = EitherT (Int, String) IO result
  route Proxy action _globalPathInfo request respond
    | null (pathInfo request) && requestMethod request == methodGet = do
        e <- runEitherT action
        respond $ Just $ case e of
          Right output ->
            responseLBS ok200 [("Content-Type", "application/json")] (encode output)
          Left (status, message) ->
            responseLBS (mkStatus status (cs message)) [] (cs message)
    | otherwise = respond Nothing

instance FromJSON result => HasClient (Get result) where
  type Client (Get result) = URI -> EitherT String IO result
  clientWithRoute Proxy req uri = do
    innerRequest <- liftIO $ reqToRequest req uri

    innerResponse <- liftIO $ __withGlobalManager $ \ manager ->
      Client.httpLbs innerRequest manager
    when (Client.responseStatus innerResponse /= ok200) $
      left ("HTTP GET request failed with status: " ++ show (Client.responseStatus innerResponse))
    maybe (left "HTTP GET request returned invalid json") return $
      decode' (Client.responseBody innerResponse)

instance ToSample a => HasDocs (Get a) where
  docsFor Proxy (endpoint, action) =
    single endpoint' action'

    where endpoint' = endpoint & method .~ DocGET
          action' = action & response.respBody .~ toSample p
          p = Proxy :: Proxy a
