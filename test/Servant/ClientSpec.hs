{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fcontext-stack=25 #-}
module Servant.ClientSpec where

import Control.Concurrent
import Control.Exception
import Control.Monad.Trans.Either
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Char
import Data.Foldable (forM_)
import Data.Monoid
import Data.Proxy
import qualified Data.Text as T
import GHC.Generics
import Network.HTTP.Media
import Network.HTTP.Types
import Network.Socket
import Network.Wai
import Network.Wai.Handler.Warp
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Servant.API
import Servant.API.ContentTypes
import Servant.Client
import Servant.Server

-- * test data types

data Person = Person {
  name :: String,
  age :: Integer
 }
  deriving (Eq, Show, Generic)

instance ToJSON Person
instance FromJSON Person

instance ToFormUrlEncoded Person where
    toFormUrlEncoded Person{..} =
        [("name", T.pack name), ("age", T.pack (show age))]

lookupEither :: (Show a, Eq a) => a -> [(a,b)] -> Either String b
lookupEither x xs = do
    maybe (Left $ "could not find key " <> show x) return $ lookup x xs

instance FromFormUrlEncoded Person where
    fromFormUrlEncoded xs = do
        n <- lookupEither "name" xs
        a <- lookupEither "age" xs
        return $ Person (T.unpack n) (read $ T.unpack a)


alice :: Person
alice = Person "Alice" 42

type Api =
       "get" :> Get '[JSON] Person
  :<|> "delete" :> Delete
  :<|> "capture" :> Capture "name" String :> Get '[JSON,FormUrlEncoded] Person
  :<|> "body" :> ReqBody '[FormUrlEncoded,JSON] Person :> Post '[JSON] Person
  :<|> "param" :> QueryParam "name" String :> Get '[FormUrlEncoded,JSON] Person
  :<|> "params" :> QueryParams "names" String :> Get '[JSON] [Person]
  :<|> "flag" :> QueryFlag "flag" :> Get '[JSON] Bool
  :<|> "matrixparam" :> MatrixParam "name" String :> Get '[JSON] Person
  :<|> "matrixparams" :> MatrixParams "name" String :> Get '[JSON] [Person]
  :<|> "matrixflag" :> MatrixFlag "flag" :> Get '[JSON] Bool
  :<|> "rawSuccess" :> Raw
  :<|> "rawFailure" :> Raw
  :<|> "multiple" :>
            Capture "first" String :>
            QueryParam "second" Int :>
            QueryFlag "third" :>
            ReqBody '[JSON] [(String, [Rational])] :>
            Get '[JSON] (String, Maybe Int, Bool, [(String, [Rational])])
api :: Proxy Api
api = Proxy

server :: Application
server = serve api (
       return alice
  :<|> return ()
  :<|> (\ name -> return $ Person name 0)
  :<|> return
  :<|> (\ name -> case name of
                   Just "alice" -> return alice
                   Just name -> left (400, name ++ " not found")
                   Nothing -> left (400, "missing parameter"))
  :<|> (\ names -> return (zipWith Person names [0..]))
  :<|> return
  :<|> (\ name -> case name of
                   Just "alice" -> return alice
                   Just name -> left (400, name ++ " not found")
                   Nothing -> left (400, "missing parameter"))
  :<|> (\ names -> return (zipWith Person names [0..]))
  :<|> return
  :<|> (\ _request respond -> respond $ responseLBS ok200 [] "rawSuccess")
  :<|> (\ _request respond -> respond $ responseLBS badRequest400 [] "rawFailure")
  :<|> \ a b c d -> return (a, b, c, d)
 )

withServer :: (BaseUrl -> IO a) -> IO a
withServer action = withWaiDaemon (return server) action

getGet :: BaseUrl -> EitherT String IO Person
getDelete :: BaseUrl -> EitherT String IO ()
getCapture :: String -> BaseUrl -> EitherT String IO Person
getBody :: Person -> BaseUrl -> EitherT String IO Person
getQueryParam :: Maybe String -> BaseUrl -> EitherT String IO Person
getQueryParams :: [String] -> BaseUrl -> EitherT String IO [Person]
getQueryFlag :: Bool -> BaseUrl -> EitherT String IO Bool
getMatrixParam :: Maybe String -> BaseUrl -> EitherT String IO Person
getMatrixParams :: [String] -> BaseUrl -> EitherT String IO [Person]
getMatrixFlag :: Bool -> BaseUrl -> EitherT String IO Bool
getRawSuccess :: Method -> BaseUrl -> EitherT String IO (Int, ByteString, MediaType)
getRawFailure :: Method -> BaseUrl -> EitherT String IO (Int, ByteString, MediaType)
getMultiple :: String -> Maybe Int -> Bool -> [(String, [Rational])]
  -> BaseUrl
  -> EitherT String IO (String, Maybe Int, Bool, [(String, [Rational])])
(     getGet
 :<|> getDelete
 :<|> getCapture
 :<|> getBody
 :<|> getQueryParam
 :<|> getQueryParams
 :<|> getQueryFlag
 :<|> getMatrixParam
 :<|> getMatrixParams
 :<|> getMatrixFlag
 :<|> getRawSuccess
 :<|> getRawFailure
 :<|> getMultiple)
    = client api

spec :: Spec
spec = do
  it "Servant.API.Get" $ withServer $ \ host -> do
    runEitherT (getGet host) `shouldReturn` Right alice

  it "Servant.API.Delete" $ withServer $ \ host -> do
    runEitherT (getDelete host) `shouldReturn` Right ()

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

{-
  it "Servant.API.MatrixParam" $ withServer $ \ host -> do
    runEitherT (getMatrixParam (Just "alice") host) `shouldReturn` Right alice
    Left result <- runEitherT (getMatrixParam (Just "bob") host)
    result `shouldContain` "bob not found"

  it "Servant.API.MatrixParam.MatrixParams" $ withServer $ \ host -> do
    runEitherT (getMatrixParams [] host) `shouldReturn` Right []
    runEitherT (getMatrixParams ["alice", "bob"] host)
      `shouldReturn` Right [Person "alice" 0, Person "bob" 1]

  context "Servant.API.MatrixParam.MatrixFlag" $
    forM_ [False, True] $ \ flag ->
    it (show flag) $ withServer $ \ host -> do
      runEitherT (getMatrixFlag flag host) `shouldReturn` Right flag
-}

  it "Servant.API.Raw on success" $ withServer $ \ host -> do
    runEitherT (getRawSuccess methodGet host) `shouldReturn` Right (200, "rawSuccess", "application"//"octet-stream")

  it "Servant.API.Raw on failure" $ withServer $ \ host -> do
    runEitherT (getRawFailure methodGet host) `shouldReturn` Right (400, "rawFailure", "application"//"octet-stream")

  modifyMaxSuccess (const 20) $ do
    it "works for a combination of Capture, QueryParam, QueryFlag and ReqBody" $
      property $ forAllShrink pathGen shrink $ \(NonEmpty cap) num flag body ->
        ioProperty $ do
          withServer $ \ host -> do
            result <- runEitherT (getMultiple cap num flag body host)
            return $
              result === Right (cap, num, flag, body)


  context "client correctly handles error status codes" $ do
    let test :: (WrappedApi, String) -> Spec
        test (WrappedApi api, desc) =
          it desc $
          withWaiDaemon (return (serve api (left (500, "error message")))) $
          \ host -> do
            let getResponse :: BaseUrl -> EitherT String IO ()
                getResponse = client api
            Left result <- runEitherT (getResponse host)
            result `shouldContain` "error message"
    mapM_ test $
      (WrappedApi (Proxy :: Proxy Delete), "Delete") :
      (WrappedApi (Proxy :: Proxy (Get '[JSON] ())), "Delete") :
      (WrappedApi (Proxy :: Proxy (Post '[JSON] ())), "Delete") :
      (WrappedApi (Proxy :: Proxy (Put '[JSON] ())), "Delete") :
      []

data WrappedApi where
  WrappedApi :: (HasServer api, Server api ~ EitherT (Int, String) IO a,
                 HasClient api, Client api ~ (BaseUrl -> EitherT String IO ())) =>
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

pathGen :: Gen (NonEmptyList Char)
pathGen = fmap NonEmpty path
 where
  path = listOf1 $ elements $
    filter (not . (`elem` "?%[]/#;")) $
    filter isPrint $
    map chr [0..127]
