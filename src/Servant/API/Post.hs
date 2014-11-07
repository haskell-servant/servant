{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Servant.API.Post where

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

-- | Endpoint for POST requests. The type variable represents the type of the
-- response body (not the request body, use 'Servant.API.RQBody.RQBody' for
-- that).
data Post a
  deriving Typeable

instance ToJSON a => HasServer (Post a) where
  type Server (Post a) = EitherT (Int, String) IO a

  route Proxy action request respond
    | null (pathInfo request) && requestMethod request == methodPost = do
        e <- runEitherT action
        respond . succeedWith $ case e of
          Right out ->
            responseLBS status201 [("Content-Type", "application/json")] (encode out)
          Left (status, message) ->
            responseLBS (mkStatus status (cs message)) [] (cs message)
    | null (pathInfo request) && requestMethod request /= methodPost =
        respond $ failWith WrongMethod
    | otherwise = respond $ failWith NotFound

instance FromJSON a => HasClient (Post a) where
  type Client (Post a) = BaseUrl -> EitherT String IO a

  clientWithRoute Proxy req uri =
    performRequest methodPost req 201 uri

instance ToSample a => HasDocs (Post a) where
  docsFor Proxy (endpoint, action) =
    single endpoint' action'

    where endpoint' = endpoint & method .~ DocPOST

          action' = action & response.respBody .~ toSample p
                           & response.respStatus .~ 201

          p = Proxy :: Proxy a
