{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -freduction-depth=100 #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Servant.ClientSpec (spec, Person(..), startWaiApp, endWaiApp) where

import           Prelude ()
import           Prelude.Compat

import           Control.Arrow
                 (left)
import           Control.Concurrent
                 (ThreadId, forkIO, killThread)
import           Control.DeepSeq
                 (NFData (..))
import           Control.Exception
                 (bracket, fromException, IOException)
import           Control.Monad.Error.Class
                 (throwError)
import           Data.Aeson
import           Data.Char
                 (chr, isPrint)
import           Data.Foldable
                 (forM_, toList)
import           Data.Maybe
                 (isJust)
import           Data.Monoid ()
import           Data.Proxy
import           Data.Semigroup
                 ((<>))
import           GHC.Generics
                 (Generic)
import qualified Network.HTTP.Types                   as HTTP
import           Network.Socket
import qualified Network.Wai                          as Wai
import           Network.Wai.Handler.Warp
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck
import           Web.FormUrlEncoded
                 (FromForm, ToForm)

import           Servant.API
                 ((:<|>) ((:<|>)), (:>), AuthProtect, BasicAuth,
                 BasicAuthData (..), Capture, CaptureAll, Delete,
                 DeleteNoContent, EmptyAPI, FormUrlEncoded, Get, Header,
                 Headers, JSON, NoContent (NoContent), Post, Put, QueryFlag,
                 QueryParam, QueryParams, Raw, ReqBody, addHeader, getHeaders)
import qualified Servant.Client.Core.Auth    as Auth
import qualified Servant.Client.Core.Request as Req
import           Servant.HttpStreams
import           Servant.Server
import           Servant.Server.Experimental.Auth
import           Servant.Test.ComprehensiveAPI

-- This declaration simply checks that all instances are in place.
_ = client comprehensiveAPIWithoutStreaming

spec :: Spec
spec = describe "Servant.HttpStreams" $ do
    successSpec
    failSpec
    wrappedApiSpec
    basicAuthSpec
    genAuthSpec
    hoistClientSpec
    connectionErrorSpec

-- * test data types

data Person = Person
  { _name :: String
  , _age  :: Integer
  } deriving (Eq, Show, Generic)

instance NFData Person where
    rnf (Person n a) = rnf n `seq` rnf a

instance ToJSON Person
instance FromJSON Person

instance ToForm Person
instance FromForm Person

instance Arbitrary Person where
  arbitrary = Person <$> arbitrary <*> arbitrary

alice :: Person
alice = Person "Alice" 42

carol :: Person
carol = Person "Carol" 17

type TestHeaders = '[Header "X-Example1" Int, Header "X-Example2" String]

type Api =
  Get '[JSON] Person
  :<|> "get" :> Get '[JSON] Person
  :<|> "deleteEmpty" :> DeleteNoContent
  :<|> "capture" :> Capture "name" String :> Get '[JSON,FormUrlEncoded] Person
  :<|> "captureAll" :> CaptureAll "names" String :> Get '[JSON] [Person]
  :<|> "body" :> ReqBody '[FormUrlEncoded,JSON] Person :> Post '[JSON] Person
  :<|> "param" :> QueryParam "name" String :> Get '[FormUrlEncoded,JSON] Person
  :<|> "params" :> QueryParams "names" String :> Get '[JSON] [Person]
  :<|> "flag" :> QueryFlag "flag" :> Get '[JSON] Bool
  :<|> "rawSuccess" :> Raw
  :<|> "rawFailure" :> Raw
  :<|> "multiple" :>
            Capture "first" String :>
            QueryParam "second" Int :>
            QueryFlag "third" :>
            ReqBody '[JSON] [(String, [Rational])] :>
            Get '[JSON] (String, Maybe Int, Bool, [(String, [Rational])])
  :<|> "headers" :> Get '[JSON] (Headers TestHeaders Bool)
  :<|> "deleteContentType" :> DeleteNoContent
  :<|> "redirectWithCookie" :> Raw
  :<|> "empty" :> EmptyAPI

api :: Proxy Api
api = Proxy

getRoot         :: ClientM Person
getGet          :: ClientM Person
getDeleteEmpty  :: ClientM NoContent
getCapture      :: String -> ClientM Person
getCaptureAll   :: [String] -> ClientM [Person]
getBody         :: Person -> ClientM Person
getQueryParam   :: Maybe String -> ClientM Person
getQueryParams  :: [String] -> ClientM [Person]
getQueryFlag    :: Bool -> ClientM Bool
getRawSuccess   :: HTTP.Method -> ClientM Response
getRawFailure   :: HTTP.Method -> ClientM Response
getMultiple     :: String -> Maybe Int -> Bool -> [(String, [Rational])]
  -> ClientM (String, Maybe Int, Bool, [(String, [Rational])])
getRespHeaders  :: ClientM (Headers TestHeaders Bool)
getDeleteContentType :: ClientM NoContent
_getRedirectWithCookie :: HTTP.Method -> ClientM Response

getRoot
  :<|> getGet
  :<|> getDeleteEmpty
  :<|> getCapture
  :<|> getCaptureAll
  :<|> getBody
  :<|> getQueryParam
  :<|> getQueryParams
  :<|> getQueryFlag
  :<|> getRawSuccess
  :<|> getRawFailure
  :<|> getMultiple
  :<|> getRespHeaders
  :<|> getDeleteContentType
  :<|> _getRedirectWithCookie
  :<|> EmptyClient = client api

server :: Application
server = serve api (
       return carol
  :<|> return alice
  :<|> return NoContent
  :<|> (\ name -> return $ Person name 0)
  :<|> (\ names -> return (zipWith Person names [0..]))
  :<|> return
  :<|> (\ name -> case name of
                   Just "alice" -> return alice
                   Just n -> throwError $ ServerError 400 (n ++ " not found") "" []
                   Nothing -> throwError $ ServerError 400 "missing parameter" "" [])
  :<|> (\ names -> return (zipWith Person names [0..]))
  :<|> return
  :<|> (Tagged $ \ _request respond -> respond $ Wai.responseLBS HTTP.ok200 [] "rawSuccess")
  :<|> (Tagged $ \ _request respond -> respond $ Wai.responseLBS HTTP.badRequest400 [] "rawFailure")
  :<|> (\ a b c d -> return (a, b, c, d))
  :<|> (return $ addHeader 1729 $ addHeader "eg2" True)
  :<|> return NoContent
  :<|> (Tagged $ \ _request respond -> respond $ Wai.responseLBS HTTP.found302 [("Location", "testlocation"), ("Set-Cookie", "testcookie=test")] "")
  :<|> emptyServer)

type FailApi =
       "get" :> Raw
  :<|> "capture" :> Capture "name" String :> Raw
  :<|> "body" :> Raw
failApi :: Proxy FailApi
failApi = Proxy

failServer :: Application
failServer = serve failApi (
       (Tagged $ \ _request respond -> respond $ Wai.responseLBS HTTP.ok200 [] "")
  :<|> (\ _capture -> Tagged $ \_request respond -> respond $ Wai.responseLBS HTTP.ok200 [("content-type", "application/json")] "")
  :<|> (Tagged $ \_request respond -> respond $ Wai.responseLBS HTTP.ok200 [("content-type", "fooooo")] "")
 )

-- * basic auth stuff

type BasicAuthAPI =
       BasicAuth "foo-realm" () :> "private" :> "basic" :> Get '[JSON] Person

basicAuthAPI :: Proxy BasicAuthAPI
basicAuthAPI = Proxy

basicAuthHandler :: BasicAuthCheck ()
basicAuthHandler =
  let check (BasicAuthData username password) =
        if username == "servant" && password == "server"
        then return (Authorized ())
        else return Unauthorized
  in BasicAuthCheck check

basicServerContext :: Context '[ BasicAuthCheck () ]
basicServerContext = basicAuthHandler :. EmptyContext

basicAuthServer :: Application
basicAuthServer = serveWithContext basicAuthAPI basicServerContext (const (return alice))

-- * general auth stuff

type GenAuthAPI =
  AuthProtect "auth-tag" :> "private" :> "auth" :> Get '[JSON] Person

genAuthAPI :: Proxy GenAuthAPI
genAuthAPI = Proxy

type instance AuthServerData (AuthProtect "auth-tag") = ()
type instance Auth.AuthClientData (AuthProtect "auth-tag") = ()

genAuthHandler :: AuthHandler Wai.Request ()
genAuthHandler =
  let handler req = case lookup "AuthHeader" (Wai.requestHeaders req) of
        Nothing -> throwError (err401 { errBody = "Missing auth header" })
        Just _ -> return ()
  in mkAuthHandler handler

genAuthServerContext :: Context '[ AuthHandler Wai.Request () ]
genAuthServerContext = genAuthHandler :. EmptyContext

genAuthServer :: Application
genAuthServer = serveWithContext genAuthAPI genAuthServerContext (const (return alice))

runClient :: NFData a => ClientM a -> BaseUrl -> IO (Either ClientError a)
runClient x burl = withClientEnvIO burl (runClientM x)

runClientUnsafe :: ClientM a -> BaseUrl -> IO (Either ClientError a)
runClientUnsafe x burl = withClientEnvIO burl (runClientMUnsafe x)
  where
    runClientMUnsafe x env = withClientM x env return

successSpec :: Spec
successSpec = beforeAll (startWaiApp server) $ afterAll endWaiApp $ do
    it "Servant.API.Get root" $ \(_, baseUrl) -> do
      left show <$> runClient getRoot baseUrl  `shouldReturn` Right carol

    it "Servant.API.Get" $ \(_, baseUrl) -> do
      left show <$> runClient getGet baseUrl  `shouldReturn` Right alice

    describe "Servant.API.Delete" $ do
      it "allows empty content type" $ \(_, baseUrl) -> do
        left show <$> runClient getDeleteEmpty baseUrl `shouldReturn` Right NoContent

      it "allows content type" $ \(_, baseUrl) -> do
        left show <$> runClient getDeleteContentType baseUrl `shouldReturn` Right NoContent

    it "Servant.API.Capture" $ \(_, baseUrl) -> do
      left show <$> runClient (getCapture "Paula") baseUrl `shouldReturn` Right (Person "Paula" 0)

    it "Servant.API.CaptureAll" $ \(_, baseUrl) -> do
      let expected = [(Person "Paula" 0), (Person "Peta" 1)]
      left show <$> runClient (getCaptureAll ["Paula", "Peta"]) baseUrl `shouldReturn` Right expected

    it "Servant.API.ReqBody" $ \(_, baseUrl) -> do
      let p = Person "Clara" 42
      left show <$> runClient (getBody p) baseUrl `shouldReturn` Right p

    it "Servant.API FailureResponse" $ \(_, baseUrl) -> do
      left show <$> runClient (getQueryParam (Just "alice")) baseUrl `shouldReturn` Right alice
      Left (FailureResponse req _) <- runClient (getQueryParam (Just "bob")) baseUrl
      Req.requestPath req `shouldBe` (baseUrl, "/param")
      toList (Req.requestQueryString req) `shouldBe` [("name", Just "bob")]
      Req.requestMethod req `shouldBe` HTTP.methodGet

    it "Servant.API.QueryParam" $ \(_, baseUrl) -> do
      left show <$> runClient (getQueryParam (Just "alice")) baseUrl `shouldReturn` Right alice
      Left (FailureResponse _ r) <- runClient (getQueryParam (Just "bob")) baseUrl
      responseStatusCode r `shouldBe` HTTP.Status 400 "bob not found"

    it "Servant.API.QueryParam.QueryParams" $ \(_, baseUrl) -> do
      left show <$> runClient (getQueryParams []) baseUrl `shouldReturn` Right []
      left show <$> runClient (getQueryParams ["alice", "bob"]) baseUrl
        `shouldReturn` Right [Person "alice" 0, Person "bob" 1]

    context "Servant.API.QueryParam.QueryFlag" $
      forM_ [False, True] $ \ flag -> it (show flag) $ \(_, baseUrl) -> do
        left show <$> runClient (getQueryFlag flag) baseUrl `shouldReturn` Right flag

    it "Servant.API.Raw on success" $ \(_, baseUrl) -> do
      res <- runClient (getRawSuccess HTTP.methodGet) baseUrl
      case res of
        Left e -> assertFailure $ show e
        Right r -> do
          responseStatusCode r `shouldBe` HTTP.status200
          responseBody r `shouldBe` "rawSuccess"

    it "Servant.API.Raw should return a Left in case of failure" $ \(_, baseUrl) -> do
      res <- runClient (getRawFailure HTTP.methodGet) baseUrl
      case res of
        Right _ -> assertFailure "expected Left, but got Right"
        Left (FailureResponse _ r) -> do
          responseStatusCode r `shouldBe` HTTP.status400
          responseBody r `shouldBe` "rawFailure"
        Left e -> assertFailure $ "expected FailureResponse, but got " ++ show e

    it "Returns headers appropriately" $ \(_, baseUrl) -> do
      res <- runClient getRespHeaders baseUrl
      case res of
        Left e -> assertFailure $ show e
        Right val -> getHeaders val `shouldBe` [("X-Example1", "1729"), ("X-Example2", "eg2")]

    modifyMaxSuccess (const 20) $ do
      it "works for a combination of Capture, QueryParam, QueryFlag and ReqBody" $ \(_, baseUrl) ->
        property $ forAllShrink pathGen shrink $ \(NonEmpty cap) num flag body ->
          ioProperty $ do
            result <- left show <$> runClient (getMultiple cap num flag body) baseUrl
            return $
              result === Right (cap, num, flag, body)


wrappedApiSpec :: Spec
wrappedApiSpec = describe "error status codes" $ do
  let serveW api = serve api $ throwError $ ServerError 500 "error message" "" []
  context "are correctly handled by the client" $
    let test :: (WrappedApi, String) -> Spec
        test (WrappedApi api, desc) =
          it desc $ bracket (startWaiApp $ serveW api) endWaiApp $ \(_, baseUrl) -> do
            let getResponse :: ClientM ()
                getResponse = client api
            Left (FailureResponse _ r) <- runClient getResponse baseUrl
            responseStatusCode r `shouldBe` (HTTP.Status 500 "error message")
    in mapM_ test $
        (WrappedApi (Proxy :: Proxy (Delete '[JSON] ())), "Delete") :
        (WrappedApi (Proxy :: Proxy (Get '[JSON] ())), "Get") :
        (WrappedApi (Proxy :: Proxy (Post '[JSON] ())), "Post") :
        (WrappedApi (Proxy :: Proxy (Put '[JSON] ())), "Put") :
        []

failSpec :: Spec
failSpec = beforeAll (startWaiApp failServer) $ afterAll endWaiApp $ do

    context "client returns errors appropriately" $ do
      it "reports FailureResponse" $ \(_, baseUrl) -> do
        let (_ :<|> _ :<|> getDeleteEmpty :<|> _) = client api
        Left res <- runClient getDeleteEmpty baseUrl
        case res of
          FailureResponse _ r | responseStatusCode r == HTTP.status404 -> return ()
          _ -> fail $ "expected 404 response, but got " <> show res

      it "reports DecodeFailure" $ \(_, baseUrl) -> do
        let (_ :<|> _ :<|> _ :<|> getCapture :<|> _) = client api
        Left res <- runClient (getCapture "foo") baseUrl
        case res of
          DecodeFailure _ _ -> return ()
          _ -> fail $ "expected DecodeFailure, but got " <> show res

      -- we don't catch IOException's
      xit "reports ConnectionError" $ \_ -> do
        let (getGetWrongHost :<|> _) = client api
        Left res <- runClient getGetWrongHost (BaseUrl Http "127.0.0.1" 19872 "")
        case res of
          ConnectionError _ -> return ()
          _ -> fail $ "expected ConnectionError, but got " <> show res

      it "reports UnsupportedContentType" $ \(_, baseUrl) -> do
        let (_ :<|> getGet :<|> _ ) = client api
        Left res <- runClient getGet baseUrl
        case res of
          UnsupportedContentType ("application/octet-stream") _ -> return ()
          _ -> fail $ "expected UnsupportedContentType, but got " <> show res

      it "reports InvalidContentTypeHeader" $ \(_, baseUrl) -> do
        let (_ :<|> _ :<|> _ :<|> _ :<|> _ :<|> getBody :<|> _) = client api
        Left res <- runClient (getBody alice) baseUrl
        case res of
          InvalidContentTypeHeader _ -> return ()
          _ -> fail $ "expected InvalidContentTypeHeader, but got " <> show res

data WrappedApi where
  WrappedApi :: (HasServer (api :: *) '[], Server api ~ Handler a,
                 HasClient ClientM api, Client ClientM api ~ ClientM ()) =>
    Proxy api -> WrappedApi

basicAuthSpec :: Spec
basicAuthSpec = beforeAll (startWaiApp basicAuthServer) $ afterAll endWaiApp $ do
  context "Authentication works when requests are properly authenticated" $ do

    it "Authenticates a BasicAuth protected server appropriately" $ \(_,baseUrl) -> do
      let getBasic = client basicAuthAPI
      let basicAuthData = BasicAuthData "servant" "server"
      left show <$> runClient (getBasic basicAuthData) baseUrl `shouldReturn` Right alice

  context "Authentication is rejected when requests are not authenticated properly" $ do

    it "Authenticates a BasicAuth protected server appropriately" $ \(_,baseUrl) -> do
      let getBasic = client basicAuthAPI
      let basicAuthData = BasicAuthData "not" "password"
      Left (FailureResponse _ r) <- runClient (getBasic basicAuthData) baseUrl
      responseStatusCode r `shouldBe` HTTP.Status 403 "Forbidden"

genAuthSpec :: Spec
genAuthSpec = beforeAll (startWaiApp genAuthServer) $ afterAll endWaiApp $ do
  context "Authentication works when requests are properly authenticated" $ do

    it "Authenticates a AuthProtect protected server appropriately" $ \(_, baseUrl) -> do
      let getProtected = client genAuthAPI
      let authRequest = Auth.mkAuthenticatedRequest () (\_ req -> Req.addHeader "AuthHeader" ("cool" :: String) req)
      left show <$> runClient (getProtected authRequest) baseUrl `shouldReturn` Right alice

  context "Authentication is rejected when requests are not authenticated properly" $ do

    it "Authenticates a AuthProtect protected server appropriately" $ \(_, baseUrl) -> do
      let getProtected = client genAuthAPI
      let authRequest = Auth.mkAuthenticatedRequest () (\_ req -> Req.addHeader "Wrong" ("header" :: String) req)
      Left (FailureResponse _ r) <- runClient (getProtected authRequest) baseUrl
      responseStatusCode r `shouldBe` (HTTP.Status 401 "Unauthorized")

-- * hoistClient

type HoistClientAPI = Get '[JSON] Int :<|> Capture "n" Int :> Post '[JSON] Int

hoistClientAPI :: Proxy HoistClientAPI
hoistClientAPI = Proxy

hoistClientServer :: Application -- implements HoistClientAPI
hoistClientServer = serve hoistClientAPI $ return 5 :<|> (\n -> return n)

hoistClientSpec :: Spec
hoistClientSpec = beforeAll (startWaiApp hoistClientServer) $ afterAll endWaiApp $ do
  describe "Servant.Client.hoistClient" $ do
    it "allows us to GET/POST/... requests in IO instead of ClientM" $ \(_, baseUrl) -> do
      let (getInt :<|> postInt)
            = hoistClient hoistClientAPI
                          (fmap (either (error . show) id) . flip runClientUnsafe baseUrl)
                          (client hoistClientAPI)

      getInt `shouldReturn` 5
      postInt 5 `shouldReturn` 5

-- * ConnectionError
type ConnectionErrorAPI = Get '[JSON] Int

connectionErrorAPI :: Proxy ConnectionErrorAPI
connectionErrorAPI = Proxy

connectionErrorSpec :: Spec
connectionErrorSpec = describe "Servant.Client.ClientError" $
    xit "correctly catches ConnectionErrors when the HTTP request can't go through" $ do
        let getInt = client connectionErrorAPI
        let baseUrl' = BaseUrl Http "example.invalid" 80 ""
        let isHttpError (Left (ConnectionError e)) = isJust $ fromException @IOException e
            isHttpError _ = False
        (isHttpError <$> runClient getInt baseUrl') `shouldReturn` True

-- * utils

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
  let localhost = tupleToHostAddress (127, 0, 0, 1)
  bind s (SockAddrInet defaultPort localhost)
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
