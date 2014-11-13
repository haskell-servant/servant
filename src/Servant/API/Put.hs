{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Servant.API.Put where

import Control.Monad.Trans.Either
import Data.Aeson
import Data.Proxy
import Data.String.Conversions
import Data.Typeable
import Network.HTTP.Types
import Network.Wai
import Servant.Client
import Servant.Common.BaseUrl
import Servant.Common.Req
import Servant.Docs
import Servant.Server

-- | Endpoint for PUT requests.
data Put a
  deriving Typeable

instance ToJSON a => HasServer (Put a) where
  type Server (Put a) = EitherT (Int, String) IO a

  route Proxy action request respond
    | null (pathInfo request) && requestMethod request == methodPut = do
        e <- runEitherT action
        respond . succeedWith $ case e of
          Right out ->
            responseLBS ok200 [("Content-Type", "application/json")] (encode out)
          Left (status, message) ->
            responseLBS (mkStatus status (cs message)) [] (cs message)
    | null (pathInfo request) && requestMethod request /= methodPut =
        respond $ failWith WrongMethod

    | otherwise = respond $ failWith NotFound

instance FromJSON a => HasClient (Put a) where
  type Client (Put a) = BaseUrl -> EitherT String IO a

  clientWithRoute Proxy req host =
    performRequestJSON methodPut req 200 host

instance ToSample a => HasDocs (Put a) where
  docsFor Proxy (endpoint, action) =
    single endpoint' action'

    where endpoint' = endpoint & method .~ DocPUT

          action' = action & response.respBody .~ sampleByteString p
                           & response.respStatus .~ 200

          p = Proxy :: Proxy a
