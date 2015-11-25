{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
#if !MIN_VERSION_base(4,8,0)
{-# LANGUAGE OverlappingInstances   #-}
#endif
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fcontext-stack=100 #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Servant.ClientSpec where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative        ((<$>), pure)
#endif
import           Control.Arrow              (left)
import           Control.Monad.Trans.Except (runExceptT, throwE)
import           Data.Aeson
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
import           Network.Wai                (Application, responseLBS)
import           System.IO.Unsafe           (unsafePerformIO)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck

import           Servant.API
import           Servant.Client
import           Servant.Client.TestServer
import           Servant.Server

spec :: Spec
spec = do
  runIO buildTestServer
  describe "Servant.Client" $ do
    sucessSpec
    failSpec
    errorSpec

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
  :<|> "rawSuccess" :> Raw
  :<|> "rawFailure" :> Raw
  :<|> "multiple" :>
            Capture "first" String :>
            QueryParam "second" Int :>
            QueryFlag "third" :>
            ReqBody '[JSON] [(String, [Rational])] :>
            Post '[JSON] (String, Maybe Int, Bool, [(String, [Rational])])
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
                   Just n -> throwE $ ServantErr 400 (n ++ " not found") "" []
                   Nothing -> throwE $ ServantErr 400 "missing parameter" "" [])
  :<|> (\ names -> return (zipWith Person names [0..]))
  :<|> return
  :<|> (\ _request respond -> respond $ responseLBS ok200 [] "rawSuccess")
  :<|> (\ _request respond -> respond $ responseLBS badRequest400 [] "rawFailure")
  :<|> (\ a b c d -> return (a, b, c, d))
  :<|> (return $ addHeader 1729 $ addHeader "eg2" True)
  :<|> return ()
 )

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
  :<|> (\ _request respond -> respond $ responseLBS ok200 [("content-type", "fooooo")] "")
  )

{-# NOINLINE manager #-}
manager :: C.Manager
manager = unsafePerformIO $ C.newManager C.defaultManagerSettings

sucessSpec :: Spec
sucessSpec = around (withTestServer server "server") $ do

    it "Servant.API.Get" $ \baseUrl -> do
      let getGet = getNth (Proxy :: Proxy 0) $ client api baseUrl manager
      (left show <$> runExceptT getGet) `shouldReturn` Right alice

    describe "Servant.API.Delete" $ do
      it "allows empty content type" $ \baseUrl -> do
        let getDeleteEmpty = getNth (Proxy :: Proxy 1) $ client api baseUrl manager
        (left show <$> runExceptT getDeleteEmpty) `shouldReturn` Right ()

      it "allows content type" $ \baseUrl -> do
        let getDeleteContentType = getLast $ client api baseUrl manager
        (left show <$> runExceptT getDeleteContentType) `shouldReturn` Right ()

    it "Servant.API.Capture" $ \baseUrl -> do
      let getCapture = getNth (Proxy :: Proxy 2) $ client api baseUrl manager
      (left show <$> runExceptT (getCapture "Paula")) `shouldReturn` Right (Person "Paula" 0)

    it "Servant.API.ReqBody" $ \baseUrl -> do
      let p = Person "Clara" 42
          getBody = getNth (Proxy :: Proxy 3) $ client api baseUrl manager
      (left show <$> runExceptT (getBody p)) `shouldReturn` Right p

    it "Servant.API.QueryParam" $ \baseUrl -> do
      let getQueryParam = getNth (Proxy :: Proxy 4) $ client api baseUrl manager
      left show <$> runExceptT (getQueryParam (Just "alice")) `shouldReturn` Right alice
      Left FailureResponse{..} <- runExceptT (getQueryParam (Just "bob"))
      responseStatus `shouldBe` Status 400 "bob not found"

    it "Servant.API.QueryParam.QueryParams" $ \baseUrl -> do
      let getQueryParams = getNth (Proxy :: Proxy 5) $ client api baseUrl manager
      (left show <$> runExceptT (getQueryParams [])) `shouldReturn` Right []
      (left show <$> runExceptT (getQueryParams ["alice", "bob"]))
        `shouldReturn` Right [Person "alice" 0, Person "bob" 1]

    context "Servant.API.QueryParam.QueryFlag" $
      forM_ [False, True] $ \ flag -> it (show flag) $ \baseUrl -> do
        let getQueryFlag = getNth (Proxy :: Proxy 6) $ client api baseUrl manager
        (left show <$> runExceptT (getQueryFlag flag)) `shouldReturn` Right flag

    it "Servant.API.Raw on success" $ \baseUrl -> do
      let getRawSuccess = getNth (Proxy :: Proxy 7) $ client api baseUrl manager
      res <- runExceptT (getRawSuccess methodGet)
      case res of
        Left e -> assertFailure $ show e
        Right (code, body, ct, _, response) -> do
          (code, body, ct) `shouldBe` (200, "rawSuccess", "application"//"octet-stream")
          C.responseBody response `shouldBe` body
          C.responseStatus response `shouldBe` ok200

    it "Servant.API.Raw should return a Left in case of failure" $ \baseUrl -> do
      let getRawFailure = getNth (Proxy :: Proxy 8) $ client api baseUrl manager
      res <- runExceptT (getRawFailure methodGet)
      case res of
        Right _ -> assertFailure "expected Left, but got Right"
        Left e -> do
          Servant.Client.responseStatus e `shouldBe` status400
          Servant.Client.responseBody e `shouldBe` "rawFailure"

    it "Returns headers appropriately" $ \baseUrl -> do
      let getRespHeaders = getNth (Proxy :: Proxy 10) $ client api baseUrl manager
      res <- runExceptT getRespHeaders
      case res of
        Left e -> assertFailure $ show e
        Right val -> getHeaders val `shouldBe` [("X-Example1", "1729"), ("X-Example2", "eg2")]

    modifyMaxSuccess (const 2) $ do
      it "works for a combination of Capture, QueryParam, QueryFlag and ReqBody" $ \baseUrl ->
        let getMultiple = getNth (Proxy :: Proxy 9) $ client api baseUrl manager
        in property $ forAllShrink pathGen shrink $ \(NonEmpty cap) num flag body ->
          ioProperty $ do
            result <- left show <$> runExceptT (getMultiple cap num flag body)
            return $
              result === Right (cap, num, flag, body)

type ErrorApi =
  Delete '[JSON] () :<|>
  Get '[JSON] () :<|>
  Post '[JSON] () :<|>
  Put '[JSON] ()

errorApi :: Proxy ErrorApi
errorApi = Proxy

errorServer :: Application
errorServer = serve errorApi $
  err :<|> err :<|> err :<|> err
  where
    err = throwE $ ServantErr 500 "error message" "" []

errorSpec :: Spec
errorSpec =
  around (withTestServer errorServer "errorServer") $ do
    describe "error status codes" $
      it "reports error statuses correctly" $ \baseUrl -> do
        let delete :<|> get :<|> post :<|> put =
              client errorApi baseUrl manager
            actions = [delete, get, post, put]
        forM_ actions $ \ clientAction -> do
          Left FailureResponse{..} <- runExceptT clientAction
          responseStatus `shouldBe` Status 500 "error message"

failSpec :: Spec
failSpec = around (withTestServer failServer "failServer") $ do

    context "client returns errors appropriately" $ do
      it "reports FailureResponse" $ \baseUrl -> do
        let (_ :<|> getDeleteEmpty :<|> _) = client api baseUrl manager
        Left res <- runExceptT getDeleteEmpty
        case res of
          FailureResponse (Status 404 "Not Found") _ _ -> return ()
          _ -> fail $ "expected 404 response, but got " <> show res

      it "reports DecodeFailure" $ \baseUrl -> do
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

      it "reports UnsupportedContentType" $ \baseUrl -> do
        let (getGet :<|> _ ) = client api baseUrl manager
        Left res <- runExceptT getGet
        case res of
          UnsupportedContentType ("application/octet-stream") _ -> return ()
          _ -> fail $ "expected UnsupportedContentType, but got " <> show res

      it "reports InvalidContentTypeHeader" $ \baseUrl -> do
        let (_ :<|> _ :<|> _ :<|> getBody :<|> _) = client api baseUrl manager
        Left res <- runExceptT (getBody alice)
        case res of
          InvalidContentTypeHeader "fooooo" _ -> return ()
          _ -> fail $ "expected InvalidContentTypeHeader, but got " <> show res


-- * utils

pathGen :: Gen (NonEmptyList Char)
pathGen = fmap NonEmpty path
 where
  path = listOf1 $ elements $
    filter (not . (`elem` ("?%[]/#;" :: String))) $
    filter isPrint $
    map chr [0..127]

class GetNth (n :: Nat) a b | n a -> b where
    getNth :: Proxy n -> a -> b

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
  GetNth 0 (x :<|> y) x where
      getNth _ (x :<|> _) = x

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
  (GetNth (n - 1) x y) => GetNth n (a :<|> x) y where
      getNth _ (_ :<|> x) = getNth (Proxy :: Proxy (n - 1)) x

class GetLast a b | a -> b where
    getLast :: a -> b

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
  (GetLast b c) => GetLast (a :<|> b) c where
      getLast (_ :<|> b) = getLast b

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
  GetLast a a where
      getLast a = a
