{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -freduction-depth=100 #-}

module Servant.SuccessSpec (spec) where

import Control.Arrow (left)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVar, readTVar)
import Data.Foldable (forM_, toList)
import Data.Maybe (listToMaybe)
import Data.Monoid ()
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Types as HTTP
import Prelude.Compat
import Servant.API
  ( Headers (..)
  , NoContent (NoContent)
  , WithStatus (WithStatus)
  , getHeaders
  )
import qualified Servant.Client.Core.Request as Req
import Servant.Test.ComprehensiveAPI
import Test.HUnit
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Prelude ()

import Servant.Client
import Servant.ClientTestUtils

-- This declaration simply checks that all instances are in place.
_ = client comprehensiveAPIWithoutStreaming

spec :: Spec
spec = describe "Servant.SuccessSpec" successSpec

successSpec :: Spec
successSpec = beforeAll (startWaiApp server) $ afterAll endWaiApp $ do
  describe "Servant.API.Get" $ do
    it "get root endpoint" $ \(_, baseUrl) -> left show <$> runClient getRoot baseUrl `shouldReturn` Right carol

    it "get simple endpoint" $ \(_, baseUrl) -> left show <$> runClient getGet baseUrl `shouldReturn` Right alice

    it "get redirection endpoint" $ \(_, baseUrl) -> left show <$> runClient getGet307 baseUrl `shouldReturn` Right "redirecting"

  describe "Servant.API.Delete" $ do
    it "allows empty content type" $ \(_, baseUrl) -> left show <$> runClient getDeleteEmpty baseUrl `shouldReturn` Right NoContent

    it "allows content type" $ \(_, baseUrl) -> left show <$> runClient getDeleteContentType baseUrl `shouldReturn` Right NoContent

  it "Servant.API.Capture" $ \(_, baseUrl) -> left show <$> runClient (getCapture "Paula") baseUrl `shouldReturn` Right (Person "Paula" 0)

  it "Servant.API.CaptureAll" $ \(_, baseUrl) -> do
    let expected = [Person "Paula" 0, Person "Peta" 1]
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
    forM_ [False, True] $
      \flag -> it (show flag) $ \(_, baseUrl) -> left show <$> runClient (getQueryFlag flag) baseUrl `shouldReturn` Right flag

  it "Servant.API.QueryParam.QueryString" $ \(_, baseUrl) -> do
    let qs = [("name", Just "bob"), ("age", Just "1")]
    left show <$> runClient (getQueryString qs) baseUrl `shouldReturn` Right (Person "bob" 1)

  it "Servant.API.QueryParam.DeepQuery" $ \(_, baseUrl) ->
    left show
      <$> runClient
        (getDeepQuery $ Filter 1 "bob")
        baseUrl
      `shouldReturn` Right (Person "bob" 1)

  it "Servant.API.Fragment" $ \(_, baseUrl) -> left id <$> runClient getFragment baseUrl `shouldReturn` Right alice

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

  it "Returns headers on UVerb requests" $ \(_, baseUrl) -> do
    res <- runClient getUVerbRespHeaders baseUrl
    case res of
      Left e -> assertFailure $ show e
      Right val -> case matchUnion val of
        Just (WithStatus val' :: WithStatus 200 (Headers TestHeaders Bool)) ->
          getHeaders val' `shouldBe` [("X-Example1", "1729"), ("X-Example2", "eg2")]
        Nothing -> assertFailure "unexpected alternative of union"

  it "Returns multiple Set-Cookie headers appropriately" $ \(_, baseUrl) -> do
    res <- runClient getSetCookieHeaders baseUrl
    case res of
      Left e -> assertFailure $ show e
      Right val -> getHeaders val `shouldBe` [("Set-Cookie", "cookie1"), ("Set-Cookie", "cookie2")]

  it "Returns multiple Set-Cookie headers via MultiVerb WithHeaders" $ \(_, baseUrl) -> do
    res <- runClient getMultiVerbSetCookie baseUrl
    case res of
      Left e -> assertFailure $ show e
      Right (body, (cookie1, cookie2)) -> do
        body `shouldBe` True
        cookie1 `shouldBe` "cookie1"
        cookie2 `shouldBe` "cookie2"

  it "Stores Cookie in CookieJar after a redirect" $ \(_, baseUrl) -> do
    mgr <- C.newManager C.defaultManagerSettings
    cj <- atomically . newTVar $ C.createCookieJar []
    _ <- runClientM (getRedirectWithCookie HTTP.methodGet) (ClientEnv mgr baseUrl (Just cj) defaultMakeClientRequest id)
    cookie <- listToMaybe . C.destroyCookieJar <$> atomically (readTVar cj)
    C.cookie_name <$> cookie `shouldBe` Just "testcookie"
    C.cookie_value <$> cookie `shouldBe` Just "test"

  it "Can modify the outgoing Request using the ClientEnv" $ \(_, baseUrl) -> do
    mgr <- C.newManager C.defaultManagerSettings
    -- In proper situation, extra headers should probably be visible in API type.
    -- However, testing for response timeout is difficult, so we test with something which is easy to observe
    let createClientRequest url r =
          fmap
            (\req -> req{C.requestHeaders = [("X-Added-Header", "XXX")]})
            (defaultMakeClientRequest url r)
        clientEnv = (mkClientEnv mgr baseUrl){makeClientRequest = createClientRequest}
    res <- runClientM (getRawSuccessPassHeaders HTTP.methodGet) clientEnv
    case res of
      Left e ->
        assertFailure $ show e
      Right r ->
        (("X-Added-Header", "XXX") `elem` responseHeaders r) `shouldBe` True

  modifyMaxSuccess (const 20) $ it "works for a combination of Capture, QueryParam, QueryFlag and ReqBody" $ \(_, baseUrl) ->
    property $ forAllShrink pathGen shrink $ \(NonEmpty cap) num flag body ->
      ioProperty $ do
        result <- left show <$> runClient (getMultiple cap num flag body) baseUrl
        pure $
          result === Right (cap, num, flag, body)

  context "With a route that can either return success or redirect" $ do
    it "Redirects when appropriate" $ \(_, baseUrl) -> do
      eitherResponse <- runClient (uverbGetSuccessOrRedirect True) baseUrl
      case eitherResponse of
        Left clientError -> fail $ show clientError
        Right response -> matchUnion response `shouldBe` Just (WithStatus @301 @Text "redirecting")

    it "Returns a proper response when appropriate" $ \(_, baseUrl) -> do
      eitherResponse <- runClient (uverbGetSuccessOrRedirect False) baseUrl
      case eitherResponse of
        Left clientError -> fail $ show clientError
        Right response -> matchUnion response `shouldBe` Just (WithStatus @200 alice)

  context "with a route that uses uverb but only has a single response" $
    it "returns the expected response" $ \(_, baseUrl) -> do
      eitherResponse <- runClient uverbGetCreated baseUrl
      case eitherResponse of
        Left clientError -> fail $ show clientError
        Right response -> matchUnion response `shouldBe` Just (WithStatus @201 carol)

  it "encodes URL pieces following ToHttpApiData instance" $ \(_, baseUrl) -> do
    let textOrig = "*"
    eitherResponse <- runClient (captureVerbatim $ Verbatim $ encodeUtf8 textOrig) baseUrl
    case eitherResponse of
      Left clientError -> fail $ show clientError
      Right textBack -> textBack `shouldBe` textOrig
