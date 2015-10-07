{-# LANGUAGE CPP                #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -fcontext-stack=100 #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.ClientSpec where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative        ((<$>))
#endif
import           Control.Arrow              (left)
import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Trans.Except
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
import           Network.HTTP.Types         hiding (Header)
import qualified Network.HTTP.Types         as HTTP
import           Network.Socket             hiding (Raw)
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

alice :: Person
alice = Person "Alice" 42

type TestHeaders = '[Header "X-Example1" Int, Header "X-Example2" String]

type Api =
       "get" :> Get '[JSON] Person
  :<|> "deleteEmpty" :> Delete '[] ()
  :<|> "capture" :> Capture "name" String :> Get '[JSON,FormUrlEncoded] Person
  :<|> "body" :> ReqBody '[FormUrlEncoded,JSON] Person :> Post '[JSON] Person
  :<|> "param" :> QueryParam "name" String :> Get '[FormUrlEncoded,JSON] Person
  :<|> "params" :> QueryParams "names" String :> Get '[JSON] [Person]
  :<|> "flag" :> QueryFlag "flag" :> Get '[JSON] Bool
  :<|> "matrixparam" :> MatrixParam "name" String :> Get '[JSON] Person
  :<|> "matrixparams" :> MatrixParams "name" String :> Get '[JSON] [Person]
  :<|> "matrixflag" :> MatrixFlag "flag" :> Get '[JSON] Bool
  :<|> "rawSuccess" :> Raw IO Application
  :<|> "rawFailure" :> Raw IO Application
  :<|> "multiple" :>
            Capture "first" String :>
            QueryParam "second" Int :>
            QueryFlag "third" :>
            ReqBody '[JSON] [(String, [Rational])] :>
            Get '[JSON] (String, Maybe Int, Bool, [(String, [Rational])])
  :<|> "headers" :> Get '[JSON] (Headers TestHeaders Bool)
  :<|> "deleteContentType" :> Delete '[JSON] ()
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
                   Just name -> throwE $ ServantErr 400 (name ++ " not found") "" []
                   Nothing -> throwE $ ServantErr 400 "missing parameter" "" [])
  :<|> (\ names -> return (zipWith Person names [0..]))
  :<|> return
  :<|> (\ name -> case name of
                   Just "alice" -> return alice
                   Just name -> throwE $ ServantErr 400 (name ++ " not found") "" []
                   Nothing -> throwE $ ServantErr 400 "missing parameter" "" [])
  :<|> (\ names -> return (zipWith Person names [0..]))
  :<|> return
  :<|> Raw (\ _request respond -> respond $ responseLBS ok200 [] "rawSuccess")
  :<|> Raw (\ _request respond -> respond $ responseLBS badRequest400 [] "rawFailure")
  :<|> (\ a b c d -> return (a, b, c, d))
  :<|> (return $ addHeader 1729 $ addHeader "eg2" True)
  :<|> return ()
 )

withServer :: (BaseUrl -> IO a) -> IO a
withServer action = withWaiDaemon (return server) action

type FailApi =
       "get" :> Raw IO Application
  :<|> "capture" :> Capture "name" String :> Raw IO Application
  :<|> "body" :> Raw IO Application
failApi :: Proxy FailApi
failApi = Proxy

failServer :: Application
failServer = serve failApi (
       Raw (\ _request respond -> respond $ responseLBS ok200 [] "")
  :<|> (\ _capture -> Raw (\ _request respond -> respond $ responseLBS ok200 [("content-type", "application/json")] ""))
  :<|> Raw (\_request respond -> respond $ responseLBS ok200 [("content-type", "fooooo")] "")
 )

withFailServer :: (BaseUrl -> IO a) -> IO a
withFailServer action = withWaiDaemon (return failServer) action

spec :: IO ()
spec = withServer $ \ baseUrl -> do
  manager <- C.newManager C.defaultManagerSettings
  let getGet :: ExceptT ServantError IO Person
      getDeleteEmpty :: ExceptT ServantError IO ()
      getCapture :: String -> ExceptT ServantError IO Person
      getBody :: Person -> ExceptT ServantError IO Person
      getQueryParam :: Maybe String -> ExceptT ServantError IO Person
      getQueryParams :: [String] -> ExceptT ServantError IO [Person]
      getQueryFlag :: Bool -> ExceptT ServantError IO Bool
      getMatrixParam :: Maybe String -> ExceptT ServantError IO Person
      getMatrixParams :: [String] -> ExceptT ServantError IO [Person]
      getMatrixFlag :: Bool -> ExceptT ServantError IO Bool
      getRawSuccess :: Method -> ExceptT ServantError IO (Int, ByteString, MediaType, [HTTP.Header], C.Response ByteString)
      getRawFailure :: Method -> ExceptT ServantError IO (Int, ByteString, MediaType, [HTTP.Header], C.Response ByteString)
      getMultiple :: String -> Maybe Int -> Bool -> [(String, [Rational])] -> ExceptT ServantError IO (String, Maybe Int, Bool, [(String, [Rational])])
      getRespHeaders :: ExceptT ServantError IO (Headers TestHeaders Bool)
      getDeleteContentType :: ExceptT ServantError IO ()
      (     getGet
       :<|> getDeleteEmpty
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
       :<|> getMultiple
       :<|> getRespHeaders
       :<|> getDeleteContentType)
         = client api baseUrl manager

  hspec $ do
    it "Servant.API.Get" $ do
      (left show <$> runExceptT getGet) `shouldReturn` Right alice

    describe "Servant.API.Delete" $ do
      it "allows empty content type" $ do
        (left show <$> runExceptT getDeleteEmpty) `shouldReturn` Right ()

      it "allows content type" $ do
        (left show <$> runExceptT getDeleteContentType) `shouldReturn` Right ()

    it "Servant.API.Capture" $ do
      (left show <$> runExceptT (getCapture "Paula")) `shouldReturn` Right (Person "Paula" 0)

    it "Servant.API.ReqBody" $ do
      let p = Person "Clara" 42
      (left show <$> runExceptT (getBody p)) `shouldReturn` Right p

    it "Servant.API.QueryParam" $ do
      left show <$> runExceptT (getQueryParam (Just "alice")) `shouldReturn` Right alice
      Left FailureResponse{..} <- runExceptT (getQueryParam (Just "bob"))
      responseStatus `shouldBe` Status 400 "bob not found"

    it "Servant.API.QueryParam.QueryParams" $ do
      (left show <$> runExceptT (getQueryParams [])) `shouldReturn` Right []
      (left show <$> runExceptT (getQueryParams ["alice", "bob"]))
        `shouldReturn` Right [Person "alice" 0, Person "bob" 1]

    context "Servant.API.QueryParam.QueryFlag" $
      forM_ [False, True] $ \ flag ->
      it (show flag) $ do
        (left show <$> runExceptT (getQueryFlag flag)) `shouldReturn` Right flag

    it "Servant.API.MatrixParam" $ do
      left show <$> runExceptT (getMatrixParam (Just "alice")) `shouldReturn` Right alice
      Left FailureResponse{..} <- runExceptT (getMatrixParam (Just "bob"))
      responseStatus `shouldBe` Status 400 "bob not found"

    it "Servant.API.MatrixParam.MatrixParams" $ do
      left show <$> runExceptT (getMatrixParams []) `shouldReturn` Right []
      left show <$> runExceptT (getMatrixParams ["alice", "bob"])
        `shouldReturn` Right [Person "alice" 0, Person "bob" 1]

    context "Servant.API.MatrixParam.MatrixFlag" $
      forM_ [False, True] $ \ flag ->
      it (show flag) $ do
        left show <$> runExceptT (getMatrixFlag flag) `shouldReturn` Right flag

    it "Servant.API.Raw on success" $ do
      res <- runExceptT (getRawSuccess methodGet)
      case res of
        Left e -> assertFailure $ show e
        Right (code, body, ct, _, response) -> do
          (code, body, ct) `shouldBe` (200, "rawSuccess", "application"//"octet-stream")
          C.responseBody response `shouldBe` body
          C.responseStatus response `shouldBe` ok200

    it "Servant.API.Raw on failure" $ do
      res <- runExceptT (getRawFailure methodGet)
      case res of
        Left e -> assertFailure $ show e
        Right (code, body, ct, _, response) -> do
          (code, body, ct) `shouldBe` (400, "rawFailure", "application"//"octet-stream")
          C.responseBody response `shouldBe` body
          C.responseStatus response `shouldBe` badRequest400

    it "Returns headers appropriately" $ withServer $ \ _ -> do
      res <- runExceptT getRespHeaders
      case res of
        Left e -> assertFailure $ show e
        Right val -> getHeaders val `shouldBe` [("X-Example1", "1729"), ("X-Example2", "eg2")]

    modifyMaxSuccess (const 20) $ do
      it "works for a combination of Capture, QueryParam, QueryFlag and ReqBody" $
        property $ forAllShrink pathGen shrink $ \(NonEmpty cap) num flag body ->
          ioProperty $ do
            result <- left show <$> runExceptT (getMultiple cap num flag body)
            return $
              result === Right (cap, num, flag, body)


    context "client correctly handles error status codes" $ do
      let test :: (WrappedApi, String) -> Spec
          test (WrappedApi api, desc) =
            it desc $
            withWaiDaemon (return (serve api (throwE $ ServantErr 500 "error message" "" []))) $
            \ host -> do
              let getResponse :: ExceptT ServantError IO ()
                  getResponse = client api host manager
              Left FailureResponse{..} <- runExceptT getResponse
              responseStatus `shouldBe` (Status 500 "error message")
      mapM_ test $
        (WrappedApi (Proxy :: Proxy (Delete '[JSON] ())), "Delete") :
        (WrappedApi (Proxy :: Proxy (Get '[JSON] ())), "Get") :
        (WrappedApi (Proxy :: Proxy (Post '[JSON] ())), "Post") :
        (WrappedApi (Proxy :: Proxy (Put '[JSON] ())), "Put") :
        []

failSpec :: IO ()
failSpec = withFailServer $ \ baseUrl -> do
  manager <- C.newManager C.defaultManagerSettings
  let getGet :: ExceptT ServantError IO Person
      getDeleteEmpty :: ExceptT ServantError IO ()
      getCapture :: String -> ExceptT ServantError IO Person
      getBody :: Person -> ExceptT ServantError IO Person
      (     getGet
       :<|> getDeleteEmpty
       :<|> getCapture
       :<|> getBody
       :<|> _ )
         = client api baseUrl manager
      getGetWrongHost :: ExceptT ServantError IO Person
      (getGetWrongHost :<|> _) = client api (BaseUrl Http "127.0.0.1" 19872 "") manager

  hspec $ do
    context "client returns errors appropriately" $ do
      it "reports FailureResponse" $ do
        Left res <- runExceptT getDeleteEmpty
        case res of
          FailureResponse (Status 404 "Not Found") _ _ -> return ()
          _ -> fail $ "expected 404 response, but got " <> show res

      it "reports DecodeFailure" $ do
        Left res <- runExceptT (getCapture "foo")
        case res of
          DecodeFailure _ ("application/json") _ -> return ()
          _ -> fail $ "expected DecodeFailure, but got " <> show res

      it "reports ConnectionError" $ do
        Left res <- runExceptT getGetWrongHost
        case res of
          ConnectionError _ -> return ()
          _ -> fail $ "expected ConnectionError, but got " <> show res

      it "reports UnsupportedContentType" $ do
        Left res <- runExceptT getGet
        case res of
          UnsupportedContentType ("application/octet-stream") _ -> return ()
          _ -> fail $ "expected UnsupportedContentType, but got " <> show res

      it "reports InvalidContentTypeHeader" $ do
        Left res <- runExceptT (getBody alice)
        case res of
          InvalidContentTypeHeader "fooooo" _ -> return ()
          _ -> fail $ "expected InvalidContentTypeHeader, but got " <> show res

data WrappedApi where
  WrappedApi :: (HasServer api, Server api ~ ExceptT ServantErr IO a,
                 HasClient api, Client api ~ ExceptT ServantError IO ()) =>
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
    let baseUrl = (BaseUrl Http "localhost" 80 ""){baseUrlPort = krakenPort}
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
    filter (not . (`elem` ("?%[]/#;" :: String))) $
    filter isPrint $
    map chr [0..127]
