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
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -freduction-depth=100 #-}
#else
{-# OPTIONS_GHC -fcontext-stack=100 #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

#include "overlapping-compat.h"
module Servant.ClientSpec where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative        ((<$>))
#endif
import           Control.Arrow              (left)
import           Control.Concurrent         (forkIO, killThread, ThreadId)
import           Control.Exception          (bracket)
import           Control.Monad.Trans.Except (throwE, runExceptT)
import           Data.Aeson
import qualified Data.ByteString.Lazy       as BS
import           Data.Char                  (chr, isPrint)
import           Data.Foldable              (forM_)
import           Data.Monoid                hiding (getLast)
import           Data.Proxy
import qualified Data.Text                  as T
import           GHC.Generics               (Generic)
import qualified Network.HTTP.Client        as C
import           Network.HTTP.Media
import qualified Network.HTTP.Types         as HTTP
import           Network.Socket
import           Network.Wai                (Request, requestHeaders, responseLBS)
import           Network.Wai.Handler.Warp
import           System.IO.Unsafe           (unsafePerformIO)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck

import           Servant.API
import           Servant.API.Internal.Test.ComprehensiveAPI
import           Servant.Client
import           Servant.Server
import           Servant.Server.Experimental.Auth
import qualified Servant.Common.Req         as SCR

-- This declaration simply checks that all instances are in place.
_ = client comprehensiveAPI

spec :: Spec
spec = describe "Servant.Client" $ do
    sucessSpec
    failSpec
    wrappedApiSpec
    basicAuthSpec
    genAuthSpec

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
  :<|> "deleteEmpty" :> DeleteNoContent '[JSON] NoContent
  :<|> "capture" :> Capture "name" String :> Get '[JSON,FormUrlEncoded] Person
  :<|> "captureAll" :> CaptureAll "names" String :> Get '[JSON] Person
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
  :<|> "deleteContentType" :> DeleteNoContent '[JSON] NoContent
api :: Proxy Api
api = Proxy

getGet :: C.Manager -> BaseUrl -> SCR.ClientM Person
getDeleteEmpty :: C.Manager -> BaseUrl -> SCR.ClientM NoContent
getCapture :: String -> C.Manager -> BaseUrl -> SCR.ClientM Person
getCaptureAll :: [String] -> C.Manager -> BaseUrl -> SCR.ClientM Person
getBody :: Person -> C.Manager -> BaseUrl -> SCR.ClientM Person
getQueryParam :: Maybe String -> C.Manager -> BaseUrl -> SCR.ClientM Person
getQueryParams :: [String] -> C.Manager -> BaseUrl -> SCR.ClientM [Person]
getQueryFlag :: Bool -> C.Manager -> BaseUrl -> SCR.ClientM Bool
getRawSuccess :: HTTP.Method -> C.Manager -> BaseUrl
  -> SCR.ClientM (Int, BS.ByteString, MediaType, [HTTP.Header], C.Response BS.ByteString)
getRawFailure :: HTTP.Method -> C.Manager -> BaseUrl
  -> SCR.ClientM (Int, BS.ByteString, MediaType, [HTTP.Header], C.Response BS.ByteString)
getMultiple :: String -> Maybe Int -> Bool -> [(String, [Rational])] -> C.Manager -> BaseUrl
  -> SCR.ClientM (String, Maybe Int, Bool, [(String, [Rational])])
getRespHeaders :: C.Manager -> BaseUrl -> SCR.ClientM (Headers TestHeaders Bool)
getDeleteContentType :: C.Manager -> BaseUrl -> SCR.ClientM NoContent
getGet
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
  :<|> getDeleteContentType = client api

server :: Application
server = serve api (
       return alice
  :<|> return NoContent
  :<|> (\ name -> return $ Person name 0)
  :<|> (\ (name : _) -> return $ Person name 0)
  :<|> return
  :<|> (\ name -> case name of
                   Just "alice" -> return alice
                   Just n -> throwE $ ServantErr 400 (n ++ " not found") "" []
                   Nothing -> throwE $ ServantErr 400 "missing parameter" "" [])
  :<|> (\ names -> return (zipWith Person names [0..]))
  :<|> return
  :<|> (\ _request respond -> respond $ responseLBS HTTP.ok200 [] "rawSuccess")
  :<|> (\ _request respond -> respond $ responseLBS HTTP.badRequest400 [] "rawFailure")
  :<|> (\ a b c d -> return (a, b, c, d))
  :<|> (return $ addHeader 1729 $ addHeader "eg2" True)
  :<|> return NoContent
 )


type FailApi =
       "get" :> Raw
  :<|> "capture" :> Capture "name" String :> Raw
  :<|> "body" :> Raw
failApi :: Proxy FailApi
failApi = Proxy

failServer :: Application
failServer = serve failApi (
       (\ _request respond -> respond $ responseLBS HTTP.ok200 [] "")
  :<|> (\ _capture _request respond -> respond $ responseLBS HTTP.ok200 [("content-type", "application/json")] "")
  :<|> (\_request respond -> respond $ responseLBS HTTP.ok200 [("content-type", "fooooo")] "")
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
type instance AuthClientData (AuthProtect "auth-tag") = ()

genAuthHandler :: AuthHandler Request ()
genAuthHandler =
  let handler req = case lookup "AuthHeader" (requestHeaders req) of
        Nothing -> throwE (err401 { errBody = "Missing auth header" })
        Just _ -> return ()
  in mkAuthHandler handler

genAuthServerContext :: Context '[ AuthHandler Request () ]
genAuthServerContext = genAuthHandler :. EmptyContext

genAuthServer :: Application
genAuthServer = serveWithContext genAuthAPI genAuthServerContext (const (return alice))

{-# NOINLINE manager #-}
manager :: C.Manager
manager = unsafePerformIO $ C.newManager C.defaultManagerSettings

sucessSpec :: Spec
sucessSpec = beforeAll (startWaiApp server) $ afterAll endWaiApp $ do

    it "Servant.API.Get" $ \(_, baseUrl) -> do
      (left show <$> runExceptT (getGet manager baseUrl)) `shouldReturn` Right alice

    describe "Servant.API.Delete" $ do
      it "allows empty content type" $ \(_, baseUrl) -> do
        (left show <$> runExceptT (getDeleteEmpty manager baseUrl)) `shouldReturn` Right NoContent

      it "allows content type" $ \(_, baseUrl) -> do
        (left show <$> runExceptT (getDeleteContentType manager baseUrl)) `shouldReturn` Right NoContent

    it "Servant.API.Capture" $ \(_, baseUrl) -> do
      (left show <$> runExceptT (getCapture "Paula" manager baseUrl)) `shouldReturn` Right (Person "Paula" 0)

    it "Servant.API.CaptureAll" $ \(_, baseUrl) -> do
      (left show <$> runExceptT (getCaptureAll ["Paula", "Peta"] manager baseUrl)) `shouldReturn` Right (Person "Paula" 0)

    it "Servant.API.ReqBody" $ \(_, baseUrl) -> do
      let p = Person "Clara" 42
      (left show <$> runExceptT (getBody p manager baseUrl)) `shouldReturn` Right p

    it "Servant.API.QueryParam" $ \(_, baseUrl) -> do
      left show <$> runExceptT (getQueryParam (Just "alice") manager baseUrl) `shouldReturn` Right alice
      Left FailureResponse{..} <- runExceptT (getQueryParam (Just "bob") manager baseUrl)
      responseStatus `shouldBe` HTTP.Status 400 "bob not found"

    it "Servant.API.QueryParam.QueryParams" $ \(_, baseUrl) -> do
      (left show <$> runExceptT (getQueryParams [] manager baseUrl)) `shouldReturn` Right []
      (left show <$> runExceptT (getQueryParams ["alice", "bob"] manager baseUrl))
        `shouldReturn` Right [Person "alice" 0, Person "bob" 1]

    context "Servant.API.QueryParam.QueryFlag" $
      forM_ [False, True] $ \ flag -> it (show flag) $ \(_, baseUrl) -> do
        (left show <$> runExceptT (getQueryFlag flag manager baseUrl)) `shouldReturn` Right flag

    it "Servant.API.Raw on success" $ \(_, baseUrl) -> do
      res <- runExceptT (getRawSuccess HTTP.methodGet manager baseUrl)
      case res of
        Left e -> assertFailure $ show e
        Right (code, body, ct, _, response) -> do
          (code, body, ct) `shouldBe` (200, "rawSuccess", "application"//"octet-stream")
          C.responseBody response `shouldBe` body
          C.responseStatus response `shouldBe` HTTP.ok200

    it "Servant.API.Raw should return a Left in case of failure" $ \(_, baseUrl) -> do
      res <- runExceptT (getRawFailure HTTP.methodGet manager baseUrl)
      case res of
        Right _ -> assertFailure "expected Left, but got Right"
        Left e -> do
          Servant.Client.responseStatus e `shouldBe` HTTP.status400
          Servant.Client.responseBody e `shouldBe` "rawFailure"

    it "Returns headers appropriately" $ \(_, baseUrl) -> do
      res <- runExceptT (getRespHeaders manager baseUrl)
      case res of
        Left e -> assertFailure $ show e
        Right val -> getHeaders val `shouldBe` [("X-Example1", "1729"), ("X-Example2", "eg2")]

    modifyMaxSuccess (const 20) $ do
      it "works for a combination of Capture, QueryParam, QueryFlag and ReqBody" $ \(_, baseUrl) ->
        property $ forAllShrink pathGen shrink $ \(NonEmpty cap) num flag body ->
          ioProperty $ do
            result <- left show <$> runExceptT (getMultiple cap num flag body manager baseUrl)
            return $
              result === Right (cap, num, flag, body)


wrappedApiSpec :: Spec
wrappedApiSpec = describe "error status codes" $ do
  let serveW api = serve api $ throwE $ ServantErr 500 "error message" "" []
  context "are correctly handled by the client" $
    let test :: (WrappedApi, String) -> Spec
        test (WrappedApi api, desc) =
          it desc $ bracket (startWaiApp $ serveW api) endWaiApp $ \(_, baseUrl) -> do
            let getResponse :: C.Manager -> BaseUrl -> SCR.ClientM ()
                getResponse = client api
            Left FailureResponse{..} <- runExceptT (getResponse manager baseUrl)
            responseStatus `shouldBe` (HTTP.Status 500 "error message")
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
        let (_ :<|> getDeleteEmpty :<|> _) = client api
        Left res <- runExceptT (getDeleteEmpty manager baseUrl)
        case res of
          FailureResponse (HTTP.Status 404 "Not Found") _ _ -> return ()
          _ -> fail $ "expected 404 response, but got " <> show res

      it "reports DecodeFailure" $ \(_, baseUrl) -> do
        let (_ :<|> _ :<|> getCapture :<|> _) = client api
        Left res <- runExceptT (getCapture "foo" manager baseUrl)
        case res of
          DecodeFailure _ ("application/json") _ -> return ()
          _ -> fail $ "expected DecodeFailure, but got " <> show res

      it "reports ConnectionError" $ \_ -> do
        let (getGetWrongHost :<|> _) = client api
        Left res <- runExceptT (getGetWrongHost manager (BaseUrl Http "127.0.0.1" 19872 ""))
        case res of
          ConnectionError _ -> return ()
          _ -> fail $ "expected ConnectionError, but got " <> show res

      it "reports UnsupportedContentType" $ \(_, baseUrl) -> do
        let (getGet :<|> _ ) = client api
        Left res <- runExceptT (getGet manager baseUrl)
        case res of
          UnsupportedContentType ("application/octet-stream") _ -> return ()
          _ -> fail $ "expected UnsupportedContentType, but got " <> show res

      it "reports InvalidContentTypeHeader" $ \(_, baseUrl) -> do
        let (_ :<|> _ :<|> _ :<|> _ :<|> getBody :<|> _) = client api
        Left res <- runExceptT (getBody alice manager baseUrl)
        case res of
          InvalidContentTypeHeader "fooooo" _ -> return ()
          _ -> fail $ "expected InvalidContentTypeHeader, but got " <> show res

data WrappedApi where
  WrappedApi :: (HasServer (api :: *) '[], Server api ~ Handler a,
                 HasClient api, Client api ~ (C.Manager -> BaseUrl -> SCR.ClientM ())) =>
    Proxy api -> WrappedApi

basicAuthSpec :: Spec
basicAuthSpec = beforeAll (startWaiApp basicAuthServer) $ afterAll endWaiApp $ do
  context "Authentication works when requests are properly authenticated" $ do

    it "Authenticates a BasicAuth protected server appropriately" $ \(_,baseUrl) -> do
      let getBasic = client basicAuthAPI
      let basicAuthData = BasicAuthData "servant" "server"
      (left show <$> runExceptT (getBasic basicAuthData manager baseUrl)) `shouldReturn` Right alice

  context "Authentication is rejected when requests are not authenticated properly" $ do

    it "Authenticates a BasicAuth protected server appropriately" $ \(_,baseUrl) -> do
      let getBasic = client basicAuthAPI
      let basicAuthData = BasicAuthData "not" "password"
      Left FailureResponse{..} <- runExceptT (getBasic basicAuthData manager baseUrl)
      responseStatus `shouldBe` HTTP.Status 403 "Forbidden"

genAuthSpec :: Spec
genAuthSpec = beforeAll (startWaiApp genAuthServer) $ afterAll endWaiApp $ do
  context "Authentication works when requests are properly authenticated" $ do

    it "Authenticates a AuthProtect protected server appropriately" $ \(_, baseUrl) -> do
      let getProtected = client genAuthAPI
      let authRequest = mkAuthenticateReq () (\_ req ->  SCR.addHeader "AuthHeader" ("cool" :: String) req)
      (left show <$> runExceptT (getProtected authRequest manager baseUrl)) `shouldReturn` Right alice

  context "Authentication is rejected when requests are not authenticated properly" $ do

    it "Authenticates a AuthProtect protected server appropriately" $ \(_, baseUrl) -> do
      let getProtected = client genAuthAPI
      let authRequest = mkAuthenticateReq () (\_ req ->  SCR.addHeader "Wrong" ("header" :: String) req)
      Left FailureResponse{..} <- runExceptT (getProtected authRequest manager baseUrl)
      responseStatus `shouldBe` (HTTP.Status 401 "Unauthorized")

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
