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
    serve testServerApi $
      testServerApp server :<|>
      testServerApp errorServer :<|>
      testServerApp failServer

type TestServerApi =
  "server" :> Raw :<|>
  "errorServer" :> Raw :<|>
  "failServer" :> Raw

testServerApi :: Proxy TestServerApi
testServerApi = Proxy
