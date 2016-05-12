{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Servant.Client.TestServer.GHC where

import           Control.Concurrent
import           Control.Exception
import           Network.Socket
import           Network.Wai
import           Network.Wai.Handler.Warp

import           Servant.Common.BaseUrl
import           Servant.Client.TestServer.Types

buildTestServer :: IO ()
buildTestServer = return ()

withServer :: TestServer -> (BaseUrl -> IO a) -> IO a
withServer (TestServer _ app) action =
  bracket (startWaiApp app) endWaiApp $ \ (_, url) ->
    action url

startWaiApp :: Application -> IO (ThreadId, BaseUrl)
startWaiApp app = do
    (port, socket) <- openTestSocket
    let settings = setPort port $ defaultSettings
    thread <- forkIO $ runSettingsSocket settings socket app
    return (thread, BaseUrl Http "localhost" port "")

endWaiApp :: (ThreadId, BaseUrl) -> IO ()
endWaiApp (thread, _) = killThread thread

openTestSocket :: IO (Port, Socket)
openTestSocket = do
  s <- socket AF_INET Stream defaultProtocol
  localhost <- inet_addr "127.0.0.1"
  bind s (SockAddrInet aNY_PORT localhost)
  listen s 1
  port <- socketPort s
  return (fromIntegral port, s)
