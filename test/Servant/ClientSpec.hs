{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Servant.ClientSpec where

import Control.Concurrent
import Control.Exception
import Control.Monad.Trans.Either
import Data.Char
import Data.Foldable (forM_)
import Data.Proxy
import Data.Typeable
import Network.Socket
import Network.Wai
import Network.Wai.Handler.Warp
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Servant.API
import Servant.Client
import Servant.Server
import Servant.Utils.BaseUrl

import Servant.ServerSpec

type Api =
       "get" :> Get Person
  :<|> "capture" :> Capture "name" String :> Get Person
  :<|> "body" :> ReqBody Person :> Post Person
  :<|> "param" :> QueryParam "name" String :> Get Person
  :<|> "params" :> QueryParams "names" String :> Get [Person]
  :<|> "flag" :> QueryFlag "flag" :> Get Bool
  :<|> "multiple" :>
            Capture "first" String :>
            QueryParam "second" Int :>
            QueryFlag "third" :>
            ReqBody [(String, [Rational])] :>
            Get (String, Maybe Int, Bool, [(String, [Rational])])
api :: Proxy Api
api = Proxy

server :: Application
server = serve api (
       return alice
  :<|> (\ name -> return $ Person name 0)
  :<|> return
  :<|> (\ name -> case name of
                   Just "alice" -> return alice
                   Just name -> left (400, name ++ " not found")
                   Nothing -> left (400, "missing parameter"))
  :<|> (\ names -> return (zipWith Person names [0..]))
  :<|> return
  :<|> \ a b c d -> return (a, b, c, d)
 )

withServer :: (BaseUrl -> IO a) -> IO a
withServer action = withWaiDaemon (return server) action

getGet :: BaseUrl -> EitherT String IO Person
getCapture :: String -> BaseUrl -> EitherT String IO Person
getBody :: Person -> BaseUrl -> EitherT String IO Person
getQueryParam :: Maybe String -> BaseUrl -> EitherT String IO Person
getQueryParams :: [String] -> BaseUrl -> EitherT String IO [Person]
getQueryFlag :: Bool -> BaseUrl -> EitherT String IO Bool
getMultiple :: String -> Maybe Int -> Bool -> [(String, [Rational])]
  -> BaseUrl
  -> EitherT String IO (String, Maybe Int, Bool, [(String, [Rational])])
(     getGet
 :<|> getCapture
 :<|> getBody
 :<|> getQueryParam
 :<|> getQueryParams
 :<|> getQueryFlag
 :<|> getMultiple)
    = client api

spec :: Spec
spec = do
  it "Servant.API.Get" $ withServer $ \ host -> do
    runEitherT (getGet host) `shouldReturn` Right alice

  it "Servant.API.Capture" $ withServer $ \ host -> do
    runEitherT (getCapture "Paula" host) `shouldReturn` Right (Person "Paula" 0)

  it "Servant.API.ReqBody" $ withServer $ \ host -> do
    let p = Person "Clara" 42
    runEitherT (getBody p host) `shouldReturn` Right p

  it "Servant.API.QueryParam" $ withServer $ \ host -> do
    runEitherT (getQueryParam (Just "alice") host) `shouldReturn` Right alice
    Left result <- runEitherT (getQueryParam (Just "bob") host)
    result `shouldContain` "bob not found"

  it "Servant.API.QueryParam.QueryParams" $ withServer $ \ host -> do
    runEitherT (getQueryParams [] host) `shouldReturn` Right []
    runEitherT (getQueryParams ["alice", "bob"] host)
      `shouldReturn` Right [Person "alice" 0, Person "bob" 1]

  context "Servant.API.QueryParam.QueryFlag" $
    forM_ [False, True] $ \ flag ->
    it (show flag) $ withServer $ \ host -> do
      runEitherT (getQueryFlag flag host) `shouldReturn` Right flag

  modifyMaxSuccess (const 20) $ do
    it "works for a combination of Capture, QueryParam, QueryFlag and ReqBody" $
      property $ forAllShrink pathGen shrink $ \ a -> \ b c d ->
        ioProperty $ do
          withServer $ \ host -> do
            result <- runEitherT (getMultiple a b c d host)
            return $
              result === Right (a, b, c, d)


  context "client correctly handles error status codes" $ do
    let test :: WrappedApi -> Spec
        test (WrappedApi api) =
          it (show (typeOf api)) $
          withWaiDaemon (return (serve api (left (500, "error message")))) $
          \ host -> do
            let getResponse :: BaseUrl -> EitherT String IO ()
                getResponse = client api
            Left result <- runEitherT (getResponse host)
            result `shouldContain` "error message"
    mapM_ test $
      (WrappedApi (Proxy :: Proxy Delete)) :
      (WrappedApi (Proxy :: Proxy (Get ()))) :
      (WrappedApi (Proxy :: Proxy (Post ()))) :
      (WrappedApi (Proxy :: Proxy (Put ()))) :
      []

data WrappedApi where
  WrappedApi :: (HasServer api, Server api ~ EitherT (Int, String) IO a,
                 HasClient api, Client api ~ (BaseUrl -> EitherT String IO ()),
                 Typeable api) =>
    Proxy api -> WrappedApi


-- * utils

withWaiDaemon :: IO Application -> (BaseUrl -> IO a) -> IO a
withWaiDaemon mkApplication action = do
  application <- mkApplication
  bracket (acquire application) free (\ (_, _, baseUrl) -> action baseUrl)
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
    let baseUrl = (BaseUrl Http "localhost" 80){baseUrlPort = krakenPort}
    return (thread, waitForKilled, baseUrl)
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

pathGen :: Gen String
pathGen = listOf $ elements $
  filter (not . (`elem` "?%[]/#")) $
  filter isPrint $
  map chr [0..127]
