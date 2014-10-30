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
import Servant.API
import Servant.Server

import Servant.ServerSpec

type Api =
       "get" :> Get Person
  :<|> "capture" :> Capture "name" String :> Get Person
api :: Proxy Api
api = Proxy

server :: Application
server = serve api (
       return alice
  :<|> (\ name -> return $ Person name 0)
 )

withServer :: (URIAuth -> IO a) -> IO a
withServer action = withWaiDaemon (return server) (action . mkHost "localhost")

getA :: URIAuth -> EitherT String IO Person
getB :: String -> URIAuth -> EitherT String IO Person
(getA :<|> getB) = client api

spec :: Spec
spec = do
  it "Servant.API.Get" $ withServer $ \ host -> do
    runEitherT (getA host) `shouldReturn` Right alice

  it "Servant.API.Capture" $ withServer $ \ host -> do
    runEitherT (getB "Paula" host) `shouldReturn` Right (Person "Paula" 0)


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
