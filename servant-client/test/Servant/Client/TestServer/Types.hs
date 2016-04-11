
module Servant.Client.TestServer.Types where

import           Network.Wai

data TestServer
  = TestServer {
    testServerName :: String,
    testServerApp :: Application
  }
