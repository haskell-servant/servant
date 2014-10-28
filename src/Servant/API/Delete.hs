{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.API.Delete where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Proxy
import Data.String.Conversions
import Network.HTTP.Types
import Network.URI
import Network.Wai
import Servant.Client
import Servant.Docs
import Servant.Server

import qualified Network.HTTP.Client as Client

-- | Endpoint for DELETE requests.
data Delete

instance HasServer Delete where
  type Server Delete = EitherT (Int, String) IO ()

  route Proxy action request respond
    | null (pathInfo request) && requestMethod request == methodDelete = do
        e <- runEitherT action
        respond $ Just $ case e of
          Right () ->
            responseLBS status204 [] ""
          Left (status, message) ->
            responseLBS (mkStatus status (cs message)) [] (cs message)
    | otherwise = respond Nothing

instance HasClient Delete where
  type Client Delete = URI -> EitherT String IO ()

  clientWithRoute Proxy req uri = do
    partialRequest <- liftIO $ reqToRequest req uri

    let request = partialRequest { Client.method = methodDelete
                                 }

    innerResponse <- liftIO . __withGlobalManager $ \ manager ->
      Client.httpLbs request manager

    when (Client.responseStatus innerResponse /= status204) $
      left ("HTTP DELETE request failed with status: " ++ show (Client.responseStatus innerResponse))

instance HasDocs Delete where
  docsFor Proxy (endpoint, action) =
    single endpoint' action'

    where endpoint' = endpoint & method .~ DocDELETE

          action' = action & response.respBody .~ Nothing
                           & response.respStatus .~ 204
