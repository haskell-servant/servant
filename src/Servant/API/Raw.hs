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
--
-- In addition to just letting you plug in your existing WAI 'Application's,
-- this can also be used with 'Servant.Utils.StaticFiles.serveDirectory' to serve
-- static files stored in a particular directory on your filesystem, or to serve
-- your API's documentation with 'Servant.Utils.StaticFiles.serveDocumentation'.
data Raw

-- | Just pass the request to the underlying application and serve its response.
--
-- Example:
--
-- > type MyApi = "images" :> Raw
-- >
-- > server :: Server MyApi
-- > server = serveDirectory "/var/www/images"
instance HasServer Raw where
  type Server Raw = Application
  route Proxy rawApplication request respond =
    rawApplication request (respond . succeedWith)

-- | Pick a 'Method' and specify where the server you want to query is. You get
-- back the status code and the response body as a 'ByteString'.
instance HasClient Raw where
  type Client Raw = Method -> BaseUrl -> EitherT String IO (Int, ByteString)

  clientWithRoute :: Proxy Raw -> Req -> Client Raw
  clientWithRoute Proxy req httpMethod host =
    performRequest httpMethod req (const True) host


instance HasDocs Raw where
  docsFor _proxy (endpoint, action) =
    single endpoint action
