{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.API.Put where

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

-- | Endpoint for PUT requests.
data Put a

instance ToJSON a => HasServer (Put a) where
  type Server (Put a) = EitherT (Int, String) IO a

  route Proxy action _globalPathInfo request respond
    | null (pathInfo request) && requestMethod request == methodPut = do
        e <- runEitherT action
        respond $ Just $ case e of
          Right out ->
            responseLBS ok200 [("Content-Type", "application/json")] (encode out)
          Left (status, message) ->
            responseLBS (mkStatus status (cs message)) [] (cs message)
    | otherwise = respond Nothing

instance FromJSON a => HasClient (Put a) where
  type Client (Put a) = URI -> EitherT String IO a

  clientWithRoute Proxy req uri = do
    partialRequest <- liftIO $ reqToRequest req uri

    let request = partialRequest { Client.method = methodPut
                                 }

    innerResponse <- liftIO . __withGlobalManager $ \ manager ->
      Client.httpLbs request manager

    when (Client.responseStatus innerResponse /= ok200) $
      left ("HTTP PUT request failed with status: " ++ show (Client.responseStatus innerResponse))

    maybe (left "HTTP PUT request returned invalid json") return $
      decode' (Client.responseBody innerResponse)

instance ToSample a => HasDocs (Put a) where
  docsFor Proxy (endpoint, action) =
    single endpoint' action'

    where endpoint' = endpoint & method .~ DocPUT

          action' = action & response.respBody .~ toSample p
                           & response.respStatus .~ 200

          p = Proxy :: Proxy a
