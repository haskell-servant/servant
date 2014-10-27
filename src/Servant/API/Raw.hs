{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Servant.API.Raw where

import Data.Proxy
import Network.Wai
import Servant.Server

-- | Endpoint for plugging in your own Wai 'Application's.
-- The given Application will get the original request received by the server
-- (i.e. with unmodified 'pathInfo', etc.)
data Raw

instance HasServer Raw where
  type Server Raw = Application
  route Proxy rawApplication globalPathInfo request respond =
    rawApplication request{pathInfo = globalPathInfo} (respond . Just)
