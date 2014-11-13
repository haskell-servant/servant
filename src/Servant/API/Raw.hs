{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Servant.API.Raw where

import Control.Monad.Trans.Either
import Data.ByteString.Lazy
import Data.Proxy
import Network.HTTP.Types
import Network.Wai
import Servant.Client
import Servant.Common.BaseUrl
import Servant.Common.Req
import Servant.Docs hiding (Method)
import Servant.Server

-- | Endpoint for plugging in your own Wai 'Application's.
--
-- The given 'Application' will get the request as received by the server, potentially with
-- a modified (stripped) 'pathInfo' if the 'Application' is being routed with 'Servant.API.Sub.:>'.
data Raw

instance HasServer Raw where
  type Server Raw = Application
  route Proxy rawApplication request respond =
    rawApplication request (respond . succeedWith)

instance HasClient Raw where
  type Client Raw = Method -> BaseUrl -> EitherT String IO ByteString

  clientWithRoute :: Proxy Raw -> Req -> Client Raw
  clientWithRoute Proxy req method host =
    performRequest method req (const True) host


instance HasDocs Raw where
  docsFor _proxy (endpoint, action) =
    single endpoint action
