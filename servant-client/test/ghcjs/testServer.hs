{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import           Network.Wai.Handler.Warp
import           Servant
import           System.IO

import           Servant.Client.TestServer.GHC
import           Servant.Client.TestServer.Types
import           Servant.ClientSpec

main :: IO ()
main = do
  (port, socket) <- openTestSocket
  let settings =
        setPort port $
        setBeforeMainLoop (print port >> hFlush stdout) $
        defaultSettings
  runSettingsSocket settings socket $
    serve testServerApi $ \ testServerName ->
      testServerApp $ lookupTestServer testServerName

type TestServerApi =
  Capture "testServerName" String :> Raw

testServerApi :: Proxy TestServerApi
testServerApi = Proxy
