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
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fcontext-stack=100 #-}
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
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Data.Aeson                 hiding ((.:))
import           Data.Char                  (chr, isPrint)
import           Data.Foldable              (forM_)
import           Data.Monoid                hiding (getLast)
import           Data.Proxy
import qualified Data.Text                  as T
import           GHC.Generics               (Generic)
import           GHC.TypeLits
import qualified Network.HTTP.Client        as C
import           Network.HTTP.Media
import           Network.HTTP.Types         (Status (..), badRequest400,
                                             methodGet, ok200, status400)
import           Network.Socket
import           Network.Wai                (Application, Request, requestHeaders, responseLBS)
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
import qualified Servant.Common.Req as SCR

-- This declaration simply checks that all instances are in place.
_ = client comprehensiveAPI

spec :: Spec
spec = describe "Servant.Client" $ do
    sucessSpec
    failSpec
    wrappedApiSpec
    authSpec

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


server :: Application
server = serve api EmptyConfig (
       return alice
  :<|> return NoContent
  :<|> (\ name -> return $ Person name 0)
  :<|> return
  :<|> (\ name -> case name of
                   Just "alice" -> return alice
                   Just n -> throwE $ ServantErr 400 (n ++ " not found") "" []
                   Nothing -> throwE $ ServantErr 400 "missing parameter" "" [])
  :<|> (\ names -> return (zipWith Person names [0..]))
  :<|> return
  :<|> (\ _request respond -> respond $ responseLBS ok200 [] "rawSuccess")
  :<|> (\ _request respond -> respond $ responseLBS badRequest400 [] "rawFailure")
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
failServer = serve failApi EmptyConfig (
       (\ _request respond -> respond $ responseLBS ok200 [] "")
  :<|> (\ _capture _request respond -> respond $ responseLBS ok200 [("content-type", "application/json")] "")
  :<|> (\_request respond -> respond $ responseLBS ok200 [("content-type", "fooooo")] "")
 )

--  auth stuff
type AuthAPI =
       BasicAuth "foo-realm" :> "private" :> "basic" :> Get '[JSON] Person
  :<|> AuthProtect "auth-tag" :> "private" :> "auth" :> Get '[JSON] Person

authAPI :: Proxy AuthAPI
authAPI = Proxy

type instance AuthReturnType (BasicAuth "foo-realm")  = ()
type instance AuthReturnType (AuthProtect "auth-tag") = ()
type instance AuthClientData (AuthProtect "auth-tag") = ()

basicAuthHandler :: BasicAuthCheck ()
basicAuthHandler =
  let check username password =
        if username == "servant" && password == "server"
        then return (Authorized ())
        else return Unauthorized
  in BasicAuthCheck check

authHandler :: AuthHandler Request ()
authHandler =
  let handler req = case lookup "AuthHeader" (requestHeaders req) of
        Nothing -> throwE (err401 { errBody = "Missing auth header"
                                  , errReasonPhrase = "denied!"
                                  })
        Just _ -> return ()
  in mkAuthHandler handler

serverConfig :: Config '[ BasicAuthCheck ()
                        , AuthHandler Request ()
                        ]
serverConfig = basicAuthHandler :. authHandler :. EmptyConfig

authServer :: Application
authServer = serve authAPI serverConfig (const (return alice) :<|> const (return alice))

{-
     -}

{-# NOINLINE manager #-}
manager :: C.Manager
manager = unsafePerformIO $ C.newManager C.defaultManagerSettings

sucessSpec :: Spec
sucessSpec = beforeAll (startWaiApp server) $ afterAll endWaiApp $ do

    it "Servant.API.Get" $ \(_, baseUrl) -> do
      let getGet = getNth (Proxy :: Proxy 0) $ client api baseUrl manager
      (left show <$> runExceptT getGet) `shouldReturn` Right alice

    describe "Servant.API.Delete" $ do
      it "allows empty content type" $ \(_, baseUrl) -> do
        let getDeleteEmpty = getNth (Proxy :: Proxy 1) $ client api baseUrl manager
        (left show <$> runExceptT getDeleteEmpty) `shouldReturn` Right NoContent

      it "allows content type" $ \(_, baseUrl) -> do
        let getDeleteContentType = getLast $ client api baseUrl manager
        (left show <$> runExceptT getDeleteContentType) `shouldReturn` Right NoContent

    it "Servant.API.Capture" $ \(_, baseUrl) -> do
      let getCapture = getNth (Proxy :: Proxy 2) $ client api baseUrl manager
      (left show <$> runExceptT (getCapture "Paula")) `shouldReturn` Right (Person "Paula" 0)

    it "Servant.API.ReqBody" $ \(_, baseUrl) -> do
      let p = Person "Clara" 42
          getBody = getNth (Proxy :: Proxy 3) $ client api baseUrl manager
      (left show <$> runExceptT (getBody p)) `shouldReturn` Right p

    it "Servant.API.QueryParam" $ \(_, baseUrl) -> do
      let getQueryParam = getNth (Proxy :: Proxy 4) $ client api baseUrl manager
      left show <$> runExceptT (getQueryParam (Just "alice")) `shouldReturn` Right alice
      Left FailureResponse{..} <- runExceptT (getQueryParam (Just "bob"))
      responseStatus `shouldBe` Status 400 "bob not found"

    it "Servant.API.QueryParam.QueryParams" $ \(_, baseUrl) -> do
      let getQueryParams = getNth (Proxy :: Proxy 5) $ client api baseUrl manager
      (left show <$> runExceptT (getQueryParams [])) `shouldReturn` Right []
      (left show <$> runExceptT (getQueryParams ["alice", "bob"]))
        `shouldReturn` Right [Person "alice" 0, Person "bob" 1]

    context "Servant.API.QueryParam.QueryFlag" $
      forM_ [False, True] $ \ flag -> it (show flag) $ \(_, baseUrl) -> do
        let getQueryFlag = getNth (Proxy :: Proxy 6) $ client api baseUrl manager
        (left show <$> runExceptT (getQueryFlag flag)) `shouldReturn` Right flag

    it "Servant.API.Raw on success" $ \(_, baseUrl) -> do
      let getRawSuccess = getNth (Proxy :: Proxy 7) $ client api baseUrl manager
      res <- runExceptT (getRawSuccess methodGet)
      case res of
        Left e -> assertFailure $ show e
        Right (code, body, ct, _, response) -> do
          (code, body, ct) `shouldBe` (200, "rawSuccess", "application"//"octet-stream")
          C.responseBody response `shouldBe` body
          C.responseStatus response `shouldBe` ok200

    it "Servant.API.Raw should return a Left in case of failure" $ \(_, baseUrl) -> do
      let getRawFailure = getNth (Proxy :: Proxy 8) $ client api baseUrl manager
      res <- runExceptT (getRawFailure methodGet)
      case res of
        Right _ -> assertFailure "expected Left, but got Right"
        Left e -> do
          Servant.Client.responseStatus e `shouldBe` status400
          Servant.Client.responseBody e `shouldBe` "rawFailure"

    it "Returns headers appropriately" $ \(_, baseUrl) -> do
      let getRespHeaders = getNth (Proxy :: Proxy 10) $ client api baseUrl manager
      res <- runExceptT getRespHeaders
      case res of
        Left e -> assertFailure $ show e
        Right val -> getHeaders val `shouldBe` [("X-Example1", "1729"), ("X-Example2", "eg2")]

    modifyMaxSuccess (const 20) $ do
      it "works for a combination of Capture, QueryParam, QueryFlag and ReqBody" $ \(_, baseUrl) ->
        let getMultiple = getNth (Proxy :: Proxy 9) $ client api baseUrl manager
        in property $ forAllShrink pathGen shrink $ \(NonEmpty cap) num flag body ->
          ioProperty $ do
            result <- left show <$> runExceptT (getMultiple cap num flag body)
            return $
              result === Right (cap, num, flag, body)


wrappedApiSpec :: Spec
wrappedApiSpec = describe "error status codes" $ do
  let serveW api = serve api EmptyConfig $ throwE $ ServantErr 500 "error message" "" []
  context "are correctly handled by the client" $
    let test :: (WrappedApi, String) -> Spec
        test (WrappedApi api, desc) =
          it desc $ bracket (startWaiApp $ serveW api) endWaiApp $ \(_, baseUrl) -> do
            let getResponse :: ExceptT ServantError IO ()
                getResponse = client api baseUrl manager
            Left FailureResponse{..} <- runExceptT getResponse
            responseStatus `shouldBe` (Status 500 "error message")
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
        let (_ :<|> getDeleteEmpty :<|> _) = client api baseUrl manager
        Left res <- runExceptT getDeleteEmpty
        case res of
          FailureResponse (Status 404 "Not Found") _ _ -> return ()
          _ -> fail $ "expected 404 response, but got " <> show res

      it "reports DecodeFailure" $ \(_, baseUrl) -> do
        let (_ :<|> _ :<|> getCapture :<|> _) = client api baseUrl manager
        Left res <- runExceptT (getCapture "foo")
        case res of
          DecodeFailure _ ("application/json") _ -> return ()
          _ -> fail $ "expected DecodeFailure, but got " <> show res

      it "reports ConnectionError" $ \_ -> do
        let (getGetWrongHost :<|> _) = client api (BaseUrl Http "127.0.0.1" 19872 "") manager
        Left res <- runExceptT getGetWrongHost
        case res of
          ConnectionError _ -> return ()
          _ -> fail $ "expected ConnectionError, but got " <> show res

      it "reports UnsupportedContentType" $ \(_, baseUrl) -> do
        let (getGet :<|> _ ) = client api baseUrl manager
        Left res <- runExceptT getGet
        case res of
          UnsupportedContentType ("application/octet-stream") _ -> return ()
          _ -> fail $ "expected UnsupportedContentType, but got " <> show res

      it "reports InvalidContentTypeHeader" $ \(_, baseUrl) -> do
        let (_ :<|> _ :<|> _ :<|> getBody :<|> _) = client api baseUrl manager
        Left res <- runExceptT (getBody alice)
        case res of
          InvalidContentTypeHeader "fooooo" _ -> return ()
          _ -> fail $ "expected InvalidContentTypeHeader, but got " <> show res

authSpec :: Spec
authSpec = beforeAll (startWaiApp authServer) $ afterAll endWaiApp $ do
  context "Authentication works when requests are properly authenticated" $ do

    it "Authenticates a BasicAuth protected server appropriately" $ \(_,baseUrl) -> do
      let (getBasic :<|> _) = client authAPI baseUrl manager
      let authData = BasicAuthData "servant" "server"
      (left show <$> runExceptT (getBasic authData)) `shouldReturn` Right alice

    it "Authenticates a AuthProtect protected server appropriately" $ \(_, baseUrl) -> do
      let (_ :<|> getProtected) = client authAPI baseUrl manager
      let authRequest = mkAuthenticateReq () (\_ req ->  SCR.addHeader "AuthHeader" ("cool" :: String) req)
      (left show <$> runExceptT (getProtected authRequest)) `shouldReturn` Right alice

  context "Authentication is rejected when requests are not authenticated properly" $ do

    it "Authenticates a BasicAuth protected server appropriately" $ \(_,baseUrl) -> do
      let (getBasic :<|> _) = client authAPI baseUrl manager
      let authData = BasicAuthData "not" "password"
      Left FailureResponse{..} <- runExceptT (getBasic authData)
      responseStatus `shouldBe` Status 403 "Forbidden"

    it "Authenticates a AuthProtect protected server appropriately" $ \(_, baseUrl) -> do
      let (_ :<|> getProtected) = client authAPI baseUrl manager
      let authRequest = mkAuthenticateReq () (\_ req ->  SCR.addHeader "Wrong" ("header" :: String) req)
      Left FailureResponse{..} <- runExceptT (getProtected authRequest)
      responseStatus `shouldBe` (Status 401 "denied")

-- * utils

data WrappedApi where
  WrappedApi :: (HasServer (api :: *) '[], Server api ~ ExceptT ServantErr IO a,
                 HasClient api, Client api ~ ExceptT ServantError IO ()) =>
    Proxy api -> WrappedApi

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

class GetNth (n :: Nat) a b | n a -> b where
    getNth :: Proxy n -> a -> b

instance OVERLAPPING_
  GetNth 0 (x :<|> y) x where
      getNth _ (x :<|> _) = x

instance OVERLAPPING_
  (GetNth (n - 1) x y) => GetNth n (a :<|> x) y where
      getNth _ (_ :<|> x) = getNth (Proxy :: Proxy (n - 1)) x

class GetLast a b | a -> b where
    getLast :: a -> b

instance OVERLAPPING_
  (GetLast b c) => GetLast (a :<|> b) c where
      getLast (_ :<|> b) = getLast b

instance OVERLAPPING_
  GetLast a a where
      getLast a = a
