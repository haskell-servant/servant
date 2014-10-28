{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Servant.API.Raw where

import Data.Proxy
import Network.Wai
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
