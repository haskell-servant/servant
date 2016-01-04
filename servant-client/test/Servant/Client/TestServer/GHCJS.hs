
module Servant.Client.TestServer.GHCJS where

import           Control.Exception
import           Safe
import           System.Exit
import           System.IO
import           System.Process

import           Servant.Client.TestServer.Types
import           Servant.Common.BaseUrl

buildTestServer :: IO ()
buildTestServer = do
  process <- spawnProcess "./test/ghcjs/build-test-server.sh" []
  ExitSuccess <- waitForProcess process
  return ()

withTestServer :: TestServer -> (BaseUrl -> IO a) -> IO a
withTestServer (TestServer testServerName _) action = do
  bracket start stop $ \ (port, _) -> action (BaseUrl Http "localhost" port ("/" ++ testServerName))
  where
    start :: IO (Int, ProcessHandle)
    start = do
      (Nothing, Just stdout, Nothing, process) <- createProcess $ (proc "./test/ghcjs/testServer" []) {
        std_out = CreatePipe
      }
      line <- hGetLine stdout
      case readMay line :: Maybe Int of
        Nothing -> error ("unparseable port: " ++ show line)
        Just port -> return (port, process)
    stop (_, process) = do
      terminateProcess process
      waitForProcess process
