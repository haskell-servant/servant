{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -fcontext-stack=25 #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.ClientSpec where

import           Control.Applicative
import qualified Control.Arrow              as Arrow
import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.ByteString.Lazy       (ByteString)
import           Data.Char
import           Data.Foldable              (forM_)
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text                  as T
import           GHC.Generics
import qualified Network.HTTP.Client        as C
import           Network.HTTP.Media
import           Network.HTTP.Types
import           Network.Socket
import           Network.Wai                hiding (Response)
import           Network.Wai.Handler.Warp
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck

import           Servant.API
import           Servant.Client
import           Servant.Server

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

deriving instance Eq ServantError

instance Eq C.HttpException where
  a == b = show a == show b

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

getGet :: BaseUrl -> EitherT ServantError IO Person
getDelete :: BaseUrl -> EitherT ServantError IO ()
getCapture :: String -> BaseUrl -> EitherT ServantError IO Person
getBody :: Person -> BaseUrl -> EitherT ServantError IO Person
getQueryParam :: Maybe String -> BaseUrl -> EitherT ServantError IO Person
getQueryParams :: [String] -> BaseUrl -> EitherT ServantError IO [Person]
getQueryFlag :: Bool -> BaseUrl -> EitherT ServantError IO Bool
getMatrixParam :: Maybe String -> BaseUrl -> EitherT ServantError IO Person
getMatrixParams :: [String] -> BaseUrl -> EitherT ServantError IO [Person]
getMatrixFlag :: Bool -> BaseUrl -> EitherT ServantError IO Bool
getRawSuccess :: Method -> BaseUrl -> EitherT ServantError IO (Int, ByteString, MediaType, C.Response ByteString)
getRawFailure :: Method -> BaseUrl -> EitherT ServantError IO (Int, ByteString, MediaType, C.Response ByteString)
getMultiple :: String -> Maybe Int -> Bool -> [(String, [Rational])]
  -> BaseUrl
  -> EitherT ServantError IO (String, Maybe Int, Bool, [(String, [Rational])])
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

type FailApi =
       "get" :> Raw
  :<|> "capture" :> Capture "name" String :> Raw
  :<|> "body" :> Raw
failApi :: Proxy FailApi
failApi = Proxy

failServer :: Application
failServer = serve failApi (
       (\ _request respond -> respond $ responseLBS ok200 [] "")
  :<|> (\ _capture _request respond -> respond $ responseLBS ok200 [("content-type", "application/json")] "")
  :<|> (\_request respond -> respond $ responseLBS ok200 [("content-type", "fooooo")] "")
 )

withFailServer :: (BaseUrl -> IO a) -> IO a
withFailServer action = withWaiDaemon (return failServer) action

spec :: Spec
spec = do
  it "Servant.API.Get" $ withServer $ \ host -> do
    (Arrow.left show <$> runEitherT (getGet host)) `shouldReturn` Right alice

  it "Servant.API.Delete" $ withServer $ \ host -> do
    (Arrow.left show <$> runEitherT (getDelete host)) `shouldReturn` Right ()

  it "Servant.API.Capture" $ withServer $ \ host -> do
    (Arrow.left show <$> runEitherT (getCapture "Paula" host)) `shouldReturn` Right (Person "Paula" 0)

  it "Servant.API.ReqBody" $ withServer $ \ host -> do
    let p = Person "Clara" 42
    (Arrow.left show <$> runEitherT (getBody p host)) `shouldReturn` Right p

  it "Servant.API.QueryParam" $ withServer $ \ host -> do
    Arrow.left show <$> runEitherT (getQueryParam (Just "alice") host) `shouldReturn` Right alice
    Left FailureResponse{..} <- runEitherT (getQueryParam (Just "bob") host)
    responseStatus `shouldBe` Status 400 "bob not found"

  it "Servant.API.QueryParam.QueryParams" $ withServer $ \ host -> do
    (Arrow.left show <$> runEitherT (getQueryParams [] host)) `shouldReturn` Right []
    (Arrow.left show <$> runEitherT (getQueryParams ["alice", "bob"] host))
      `shouldReturn` Right [Person "alice" 0, Person "bob" 1]

  context "Servant.API.QueryParam.QueryFlag" $
    forM_ [False, True] $ \ flag ->
    it (show flag) $ withServer $ \ host -> do
      (Arrow.left show <$> runEitherT (getQueryFlag flag host)) `shouldReturn` Right flag

  it "Servant.API.MatrixParam" $ withServer $ \ host -> do
    Arrow.left show <$> runEitherT (getMatrixParam (Just "alice") host) `shouldReturn` Right alice
    Left FailureResponse{..} <- runEitherT (getMatrixParam (Just "bob") host)
    responseStatus `shouldBe` Status 400 "bob not found"

  it "Servant.API.MatrixParam.MatrixParams" $ withServer $ \ host -> do
    Arrow.left show <$> runEitherT (getMatrixParams [] host) `shouldReturn` Right []
    Arrow.left show <$> runEitherT (getMatrixParams ["alice", "bob"] host)
      `shouldReturn` Right [Person "alice" 0, Person "bob" 1]

  context "Servant.API.MatrixParam.MatrixFlag" $
    forM_ [False, True] $ \ flag ->
    it (show flag) $ withServer $ \ host -> do
      Arrow.left show <$> runEitherT (getMatrixFlag flag host) `shouldReturn` Right flag

  it "Servant.API.Raw on success" $ withServer $ \ host -> do
    res <- runEitherT (getRawSuccess methodGet host)
    case res of
      Left e -> assertFailure $ show e
      Right (code, body, ct, response) -> do
        (code, body, ct) `shouldBe` (200, "rawSuccess", "application"//"octet-stream")
        C.responseBody response `shouldBe` body
        C.responseStatus response `shouldBe` ok200

  it "Servant.API.Raw on failure" $ withServer $ \ host -> do
    res <- runEitherT (getRawFailure methodGet host)
    case res of
      Left e -> assertFailure $ show e
      Right (code, body, ct, response) -> do
        (code, body, ct) `shouldBe` (400, "rawFailure", "application"//"octet-stream")
        C.responseBody response `shouldBe` body
        C.responseStatus response `shouldBe` badRequest400

  modifyMaxSuccess (const 20) $ do
    it "works for a combination of Capture, QueryParam, QueryFlag and ReqBody" $
      property $ forAllShrink pathGen shrink $ \(NonEmpty cap) num flag body ->
        ioProperty $ do
          withServer $ \ host -> do
            result <- Arrow.left show <$> runEitherT (getMultiple cap num flag body host)
            return $
              result === Right (cap, num, flag, body)


  context "client correctly handles error status codes" $ do
    let test :: (WrappedApi, String) -> Spec
        test (WrappedApi api, desc) =
          it desc $
          withWaiDaemon (return (serve api (left (500, "error message")))) $
          \ host -> do
            let getResponse :: BaseUrl -> EitherT ServantError IO ()
                getResponse = client api
            Left FailureResponse{..} <- runEitherT (getResponse host)
            responseStatus `shouldBe` (Status 500 "error message")
    mapM_ test $
      (WrappedApi (Proxy :: Proxy Delete), "Delete") :
      (WrappedApi (Proxy :: Proxy (Get '[JSON] ())), "Get") :
      (WrappedApi (Proxy :: Proxy (Post '[JSON] ())), "Post") :
      (WrappedApi (Proxy :: Proxy (Put '[JSON] ())), "Put") :
      []

  context "client returns errors appropriately" $ do
    it "reports FailureResponse" $ withFailServer $ \ host -> do
      Left res <- runEitherT (getDelete host)
      case res of
        FailureResponse (Status 404 "Not Found") _ _ -> return ()
        _ -> fail $ "expected 404 response, but got " <> show res

    it "reports DecodeFailure" $ withFailServer $ \ host -> do
      Left res <- runEitherT (getCapture "foo" host)
      case res of
        DecodeFailure _ ("application/json") _ -> return ()
        _ -> fail $ "expected DecodeFailure, but got " <> show res

    it "reports ConnectionError" $ do
      Right host <- return $ parseBaseUrl "127.0.0.1:987654"
      Left res <- runEitherT (getGet host)
      case res of
        ConnectionError (C.FailedConnectionException2 "127.0.0.1" 987654 False _) -> return ()
        _ -> fail $ "expected ConnectionError, but got " <> show res

    it "reports UnsupportedContentType" $ withFailServer $ \ host -> do
      Left res <- runEitherT (getGet host)
      case res of
        UnsupportedContentType ("application/octet-stream") _ -> return ()
        _ -> fail $ "expected UnsupportedContentType, but got " <> show res

    it "reports InvalidContentTypeHeader" $ withFailServer $ \ host -> do
      Left res <- runEitherT (getBody alice host)
      case res of
        InvalidContentTypeHeader "fooooo" _ -> return ()
        _ -> fail $ "expected InvalidContentTypeHeader, but got " <> show res

data WrappedApi where
  WrappedApi :: (HasServer (Canonicalize api), Server api ~ EitherT (Int, String) IO a,
                 HasClient (Canonicalize api), Client api ~ (BaseUrl -> EitherT ServantError IO ())) =>
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
