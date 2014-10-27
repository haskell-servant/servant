{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Servant.API.Post where

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
import Servant.Server

import qualified Network.HTTP.Client as Client

-- | Endpoint for POST requests.
data Post a

instance ToJSON a => HasServer (Post a) where
  type Server (Post a) = EitherT (Int, String) IO a

  route Proxy action _globalPathInfo request respond
    | null (pathInfo request) && requestMethod request == methodPost = do
        e <- runEitherT action
        respond $ Just $ case e of
          Right out ->
            responseLBS status201 [("Content-Type", "application/json")] (encode out)
          Left (status, message) ->
            responseLBS (mkStatus status (cs message)) [] (cs message)
    | otherwise = respond Nothing

instance FromJSON a => HasClient (Post a) where
  type Client (Post a) = URI -> EitherT String IO a

  clientWithRoute Proxy req uri = do
    partialRequest <- liftIO $ reqToRequest req uri

    let request = partialRequest { Client.method = methodPost
                                 }

    innerResponse <- liftIO . __withGlobalManager $ \ manager ->
      Client.httpLbs request manager

    when (Client.responseStatus innerResponse /= status201) $
      left ("HTTP POST request failed with status: " ++ show (Client.responseStatus innerResponse))

    maybe (left "HTTP POST request returned invalid json") return $
      decode' (Client.responseBody innerResponse)
