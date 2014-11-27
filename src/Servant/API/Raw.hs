{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Servant.API.Raw where

import Data.Proxy
import Network.Wai
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
