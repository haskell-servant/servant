{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Servant.ClientSpec where

import Control.Concurrent
import Control.Exception
import Control.Monad.Trans.Either
import Data.Proxy
import Network.Socket
import Network.URI
import Network.Wai
import Network.Wai.Handler.Warp
import Test.Hspec

import Servant.Client
import Servant.API.Sub
import Servant.API.Get
import Servant.Server

import Servant.ServerSpec

type Api =
       "a" :> Get Person
api :: Proxy Api
api = Proxy

server :: Application
server = serve api (
  return alice
 )

getA :: URIAuth -> EitherT String IO Person
getA = client api

spec :: Spec
spec = do
  it "Servant.API.Get" $ withWaiDaemon (return server) $ \ port -> do
    runEitherT (getA (mkHost "localhost" port)) `shouldReturn` Right alice


-- * utils

withWaiDaemon :: IO Application -> (Port -> IO a) -> IO a
withWaiDaemon mkApplication action = do
  application <- mkApplication
  bracket (acquire application) free (\ (_, _, port) -> action port)
 where
  acquire application = do
    (notifyStart, waitForStart) <- lvar
    (notifyKilled, waitForKilled) <- lvar
    thread <- forkIO $ (do
      (krakenPort, socket) <- openTestSocket
      let settings =
            setPort krakenPort $ -- set here just for consistency, shouldn't be
                                 -- used (it's set in the socket)
            setBeforeMainLoop (notifyStart krakenPort)
            defaultSettings
      runSettingsSocket settings socket application)
            `finally` notifyKilled ()
    krakenPort <- waitForStart
    return (thread, waitForKilled, krakenPort)
  free (thread, waitForKilled, _) = do
    killThread thread
    waitForKilled

  lvar :: IO (a -> IO (), IO a)
  lvar = do
    mvar <- newEmptyMVar
    let put = putMVar mvar
        wait = readMVar mvar
    return (put, wait)

openTestSocket :: IO (Port, Socket)
openTestSocket = do
  s <- socket AF_INET Stream defaultProtocol
  localhost <- inet_addr "127.0.0.1"
  bind s (SockAddrInet aNY_PORT localhost)
  listen s 1
  port <- socketPort s
  return (fromIntegral port, s)
