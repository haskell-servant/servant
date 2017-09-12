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

import           Control.Arrow                              (left)
import           Control.Concurrent                         (ThreadId, forkIO,
                                                             killThread)
import           Control.Exception                          (bracket)
import           Control.Monad.Error.Class                  (throwError)
import           Data.Aeson
import qualified Data.ByteString.Lazy                       as BS
import           Data.Char                                  (chr, isPrint)
import           Data.Foldable                              (forM_)
import           Data.Monoid                                hiding (getLast)
import           Data.Proxy
import qualified Generics.SOP                               as SOP
import           GHC.Generics                               (Generic)
import qualified Network.HTTP.Client                        as C
import           Network.HTTP.Media
import qualified Network.HTTP.Types                         as HTTP
import           Network.Socket
import qualified Network.Wai                                as Wai
import           Network.Wai.Handler.Warp
import           Prelude                                    ()
import           Prelude.Compat
import           System.IO.Unsafe                           (unsafePerformIO)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck
import           Web.FormUrlEncoded                         (FromForm, ToForm)

import           Servant.API                                ((:<|>) ((:<|>)),
                                                             (:>), AuthProtect,
                                                             BasicAuth,
                                                             BasicAuthData (..),
                                                             Capture,
                                                             CaptureAll, Delete,
                                                             DeleteNoContent,
                                                             EmptyAPI,
                                                             FormUrlEncoded,
                                                             Get, Header,
                                                             Headers, JSON,
                                                             NoContent, Post,
                                                             Put, QueryFlag,
                                                             QueryParam,
                                                             QueryParams,
                                                             ReqBody)
import           Servant.API.Internal.Test.ComprehensiveAPI
import           Servant.Client
{-import qualified Servant.Common.Req         as SCR-}
{-import qualified Servant.Client.HttpClient  as SCR-}
import           Servant.Server
import           Servant.Server.Experimental.Auth

-- This declaration simply checks that all instances are in place.
_ = client comprehensiveAPI

spec :: Spec
spec = describe "Servant.Client" $ do
    sucessSpec
    failSpec
    wrappedApiSpec
    basicAuthSpec
    genAuthSpec
    genericClientSpec

-- * test data types

data Person = Person
  { name :: String
  , age  :: Integer
  } deriving (Eq, Show, Generic)

instance ToJSON Person
instance FromJSON Person

instance ToForm Person
instance FromForm Person

alice :: Person
alice = Person "Alice" 42

type TestHeaders = '[Header "X-Example1" Int, Header "X-Example2" String]

type Api =
       "get" :> Get '[JSON] Person
  :<|> "deleteEmpty" :> DeleteNoContent '[JSON] NoContent
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
  :<|> "deleteContentType" :> DeleteNoContent '[JSON] NoContent
  :<|> "empty" :> EmptyAPI

api :: Proxy Api
api = Proxy

getGet          :: ClientM Person
getDeleteEmpty  :: ClientM NoContent
getCapture      :: String -> ClientM Person
getCaptureAll   :: [String] -> ClientM [Person]
getBody         :: Person -> ClientM Person
getQueryParam   :: Maybe String -> ClientM Person
getQueryParams  :: [String] -> ClientM [Person]
getQueryFlag    :: Bool -> ClientM Bool
getRawSuccess :: HTTP.Method
  -> ClientM Response
getRawFailure   :: HTTP.Method
  -> ClientM (Int, BS.ByteString, MediaType, [HTTP.Header], C.Response BS.ByteString)
getMultiple     :: String -> Maybe Int -> Bool -> [(String, [Rational])]
  -> ClientM (String, Maybe Int, Bool, [(String, [Rational])])
getRespHeaders  :: ClientM (Headers TestHeaders Bool)
getDeleteContentType :: ClientM NoContent

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
  :<|> getDeleteContentType
  :<|> EmptyClient = client api

server :: Application
server = serve api (
       return alice
  :<|> return NoContent
  :<|> (\ name -> return $ Person name 0)
  :<|> (\ names -> return (zipWith Person names [0..]))
  :<|> return
  :<|> (\ name -> case name of
                   Just "alice" -> return alice
                   Just n -> throwError $ ServantErr 400 (n ++ " not found") "" []
                   Nothing -> throwError $ ServantErr 400 "missing parameter" "" [])
  :<|> (\ names -> return (zipWith Person names [0..]))
  :<|> return
  :<|> (Tagged $ \ _request respond -> respond $ Wai.responseLBS HTTP.ok200 [] "rawSuccess")
  :<|> (Tagged $ \ _request respond -> respond $ Wai.responseLBS HTTP.badRequest400 [] "rawFailure")
  :<|> (\ a b c d -> return (a, b, c, d))
  :<|> (return $ addHeader 1729 $ addHeader "eg2" True)
  :<|> return NoContent
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
type instance AuthClientData (AuthProtect "auth-tag") = ()

genAuthHandler :: AuthHandler Request ()
genAuthHandler =
  let handler req = case lookup "AuthHeader" (requestHeaders req) of
        Nothing -> throwError (err401 { errBody = "Missing auth header" })
        Just _ -> return ()
  in mkAuthHandler handler

genAuthServerContext :: Context '[ AuthHandler Request () ]
genAuthServerContext = genAuthHandler :. EmptyContext

genAuthServer :: Application
genAuthServer = serveWithContext genAuthAPI genAuthServerContext (const (return alice))

-- * generic client stuff

type GenericClientAPI
    = QueryParam "sqr" Int :> Get '[JSON] Int
 :<|> Capture "foo" String :> NestedAPI1

data GenericClient = GenericClient
  { getSqr          :: Maybe Int -> ClientM Int
  , mkNestedClient1 :: String -> NestedClient1
  } deriving Generic
instance SOP.Generic GenericClient
instance (Client ClientM GenericClientAPI ~ client) => ClientLike client GenericClient

type NestedAPI1
    = QueryParam "int" Int :> NestedAPI2
 :<|> QueryParam "id" Char :> Get '[JSON] Char

data NestedClient1 = NestedClient1
  { mkNestedClient2 :: Maybe Int -> NestedClient2
  , idChar          :: Maybe Char -> ClientM Char
  } deriving Generic
instance SOP.Generic NestedClient1
instance (Client ClientM NestedAPI1 ~ client) => ClientLike client NestedClient1

type NestedAPI2
    = "sum"  :> Capture "first" Int :> Capture "second" Int :> Get '[JSON] Int
 :<|> "void" :> Post '[JSON] ()

data NestedClient2 = NestedClient2
  { getSum    :: Int -> Int -> ClientM Int
  , doNothing :: ClientM ()
  } deriving Generic
instance SOP.Generic NestedClient2
instance (Client ClientM NestedAPI2 ~ client) => ClientLike client NestedClient2

genericClientServer :: Application
genericClientServer = serve (Proxy :: Proxy GenericClientAPI) (
       (\ mx -> case mx of
                  Just x -> return (x*x)
                  Nothing -> throwError $ ServantErr 400 "missing parameter" "" []
       )
  :<|> nestedServer1
 )
  where
    nestedServer1 _str = nestedServer2 :<|> (maybe (throwError $ ServantErr 400 "missing parameter" "" []) return)
    nestedServer2 _int = (\ x y -> return (x + y)) :<|> return ()

{-# NOINLINE manager' #-}
manager' :: C.Manager
manager' = unsafePerformIO $ C.newManager C.defaultManagerSettings

runClient x = runClientM x (ClientEnv manager' baseUrl)

sucessSpec :: Spec
sucessSpec = beforeAll (startWaiApp server) $ afterAll endWaiApp $ do

    it "Servant.API.Get" $ \(_, baseUrl) -> do
      (left show <$> runClient getGet)  `shouldReturn` Right alice

    describe "Servant.API.Delete" $ do
      it "allows empty content type" $ \(_, baseUrl) -> do
        (left show <$> (runClient getDeleteEmpty)) `shouldReturn` Right NoContent

      it "allows content type" $ \(_, baseUrl) -> do
        (left show <$> (runClient getDeleteContentType)) `shouldReturn` Right NoContent

    it "Servant.API.Capture" $ \(_, baseUrl) -> do
      (left show <$> (runClient (getCapture "Paula"))) `shouldReturn` Right (Person "Paula" 0)

    it "Servant.API.CaptureAll" $ \(_, baseUrl) -> do
      let expected = [(Person "Paula" 0), (Person "Peta" 1)]
      (left show <$> (runClient (getCaptureAll ["Paula", "Peta"]))) `shouldReturn` Right expected

    it "Servant.API.ReqBody" $ \(_, baseUrl) -> do
      let p = Person "Clara" 42
      (left show <$> runClient (getBody p)) `shouldReturn` Right p

    it "Servant.API.QueryParam" $ \(_, baseUrl) -> do
      left show <$> runClient (getQueryParam (Just "alice"))  `shouldReturn` Right alice
      Left (FailureResponse r) <- runClient (getQueryParam (Just "bob"))
      responseStatusCode r `shouldBe` HTTP.Status 400 "bob not found"

    it "Servant.API.QueryParam.QueryParams" $ \(_, baseUrl) -> do
      (left show <$> runClient (getQueryParams [])) `shouldReturn` Right []
      (left show <$> runClient (getQueryParams ["alice", "bob"]))
        `shouldReturn` Right [Person "alice" 0, Person "bob" 1]

    context "Servant.API.QueryParam.QueryFlag" $
      forM_ [False, True] $ \ flag -> it (show flag) $ \(_, baseUrl) -> do
        (left show <$> runClient (getQueryFlag flag)) `shouldReturn` Right flag

    it "Servant.API.Raw on success" $ \(_, baseUrl) -> do
      res <- runClient (getRawSuccess HTTP.methodGet)
      case res of
        Left e -> assertFailure $ show e
        Right (code, body, ct, _, response) -> do
          (code, body, ct) `shouldBe` (200, "rawSuccess", "application"//"octet-stream")
          C.responseBody response `shouldBe` body
          C.responseStatus response `shouldBe` HTTP.ok200

    it "Servant.API.Raw should return a Left in case of failure" $ \(_, baseUrl) -> do
      res <- runClient (getRawFailure HTTP.methodGet)
      case res of
        Right _ -> assertFailure "expected Left, but got Right"
        Left (FailureResponse r) -> do
          responseStatusCode r `shouldBe` HTTP.status400
          responseBody r `shouldBe` "rawFailure"
        Left e -> assertFailure $ "expected FailureResponse, but got " ++ show e

    it "Returns headers appropriately" $ \(_, baseUrl) -> do
      res <- runClient getRespHeaders
      case res of
        Left e -> assertFailure $ show e
        Right val -> getHeaders val `shouldBe` [("X-Example1", "1729"), ("X-Example2", "eg2")]

    modifyMaxSuccess (const 20) $ do
      it "works for a combination of Capture, QueryParam, QueryFlag and ReqBody" $ \(_, baseUrl) ->
        property $ forAllShrink pathGen shrink $ \(NonEmpty cap) num flag body ->
          ioProperty $ do
            result <- left show <$> runClient (getMultiple cap num flag body)
            return $
              result === Right (cap, num, flag, body)


wrappedApiSpec :: Spec
wrappedApiSpec = describe "error status codes" $ do
  let serveW api = serve api $ throwError $ ServantErr 500 "error message" "" []
  context "are correctly handled by the client" $
    let test :: (WrappedApi, String) -> Spec
        test (WrappedApi api, desc) =
          it desc $ bracket (startWaiApp $ serveW api) endWaiApp $ \(_, baseUrl) -> do
            let getResponse :: ClientM ()
                getResponse = client api
            Left (FailureResponse r) <- runClient getResponse
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
        let (_ :<|> getDeleteEmpty :<|> _) = client api
        Left res <- runClient getDeleteEmpty
        case res of
          FailureResponse r | responseStatusCode r == 404 -> return ()
          _ -> fail $ "expected 404 response, but got " <> show res

      it "reports DecodeFailure" $ \(_, baseUrl) -> do
        let (_ :<|> _ :<|> getCapture :<|> _) = client api
        Left res <- runClient (getCapture "foo") (ClientEnv manager baseUrl)
        case res of
          DecodeFailure _ _ -> return ()
          _ -> fail $ "expected DecodeFailure, but got " <> show res

      it "reports ConnectionError" $ \_ -> do
        let (getGetWrongHost :<|> _) = client api
        Left res <- runClientM getGetWrongHost (ClientEnv manager (BaseUrl Http "127.0.0.1" 19872 ""))
        case res of
          ConnectionError _ -> return ()
          _ -> fail $ "expected ConnectionError, but got " <> show res

      it "reports UnsupportedContentType" $ \(_, baseUrl) -> do
        let (getGet :<|> _ ) = client api
        Left res <- runClient getGet
        case res of
          UnsupportedContentType ("application/octet-stream") _ -> return ()
          _ -> fail $ "expected UnsupportedContentType, but got " <> show res

      it "reports InvalidContentTypeHeader" $ \(_, baseUrl) -> do
        let (_ :<|> _ :<|> _ :<|> _ :<|> getBody :<|> _) = client api
        Left res <- runClient (getBody alice)
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
      (left show <$> runClient (getBasic basicAuthData)) `shouldReturn` Right alice

  context "Authentication is rejected when requests are not authenticated properly" $ do

    it "Authenticates a BasicAuth protected server appropriately" $ \(_,baseUrl) -> do
      let getBasic = client basicAuthAPI
      let basicAuthData = BasicAuthData "not" "password"
      Left (FailureResponse r) <- runClient (getBasic basicAuthData)
      responseStatusCode r `shouldBe` HTTP.Status 403 "Forbidden"

genAuthSpec :: Spec
genAuthSpec = beforeAll (startWaiApp genAuthServer) $ afterAll endWaiApp $ do
  context "Authentication works when requests are properly authenticated" $ do

    it "Authenticates a AuthProtect protected server appropriately" $ \(_, baseUrl) -> do
      let getProtected = client genAuthAPI
      let authRequest = mkAuthenticateReq () (\_ req -> addHeader "AuthHeader" ("cool" :: String) req)
      (left show <$> runClient (getProtected authRequest) ) `shouldReturn` Right alice

  context "Authentication is rejected when requests are not authenticated properly" $ do

    it "Authenticates a AuthProtect protected server appropriately" $ \(_, baseUrl) -> do
      let getProtected = client genAuthAPI
      let authRequest = mkAuthenticateReq () (\_ req -> addHeader "Wrong" ("header" :: String) req)
      Left (FailureResponse r) <- runClient (getProtected authRequest)
      responseStatusCode r `shouldBe` (HTTP.Status 401 "Unauthorized")

genericClientSpec :: Spec
genericClientSpec = beforeAll (startWaiApp genericClientServer) $ afterAll endWaiApp $ do
  describe "Servant.Client.Generic" $ do

    let GenericClient{..} = mkClient (client (Proxy :: Proxy GenericClientAPI))
        NestedClient1{..} = mkNestedClient1 "example"
        NestedClient2{..} = mkNestedClient2 (Just 42)

    it "works for top-level client inClientM function" $ \(_, baseUrl) -> do
      (left show <$> (runClient (getSqr (Just 5)))) `shouldReturn` Right 25

    it "works for nested clients" $ \(_, baseUrl) -> do
      (left show <$> (runClient (idChar (Just 'c')))) `shouldReturn` Right 'c'
      (left show <$> (runClient (getSum 3 4))) `shouldReturn` Right 7
      (left show <$> (runClient doNothing )) `shouldReturn` Right ()

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
