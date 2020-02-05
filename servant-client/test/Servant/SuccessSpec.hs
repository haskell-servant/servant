{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -freduction-depth=100 #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Servant.SuccessSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Control.Arrow
                 (left)
import           Control.Concurrent.STM
                 (atomically)
import           Control.Concurrent.STM.TVar
                 (newTVar, readTVar)
import           Data.Foldable
                 (forM_, toList)
import           Data.Maybe
                 (listToMaybe)
import           Data.Monoid ()
import qualified Network.HTTP.Client                  as C
import qualified Network.HTTP.Types                   as HTTP
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.HUnit
import           Test.QuickCheck

import           Servant.API
                 (NoContent (NoContent), getHeaders)
import           Servant.Client
import qualified Servant.Client.Core.Request as Req
import           Servant.Client.Internal.HttpClient (defaultMakeClientRequest)
import           Servant.Test.ComprehensiveAPI
import           Servant.ClientTestUtils

-- This declaration simply checks that all instances are in place.
_ = client comprehensiveAPIWithoutStreaming

spec :: Spec
spec = describe "Servant.SuccessSpec" $ do
    successSpec

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

    it "Stores Cookie in CookieJar after a redirect" $ \(_, baseUrl) -> do
      mgr <- C.newManager C.defaultManagerSettings
      cj <- atomically . newTVar $ C.createCookieJar []
      _ <- runClientM (getRedirectWithCookie HTTP.methodGet) (ClientEnv mgr baseUrl (Just cj) defaultMakeClientRequest defaultAcceptStatusCode)
      cookie <- listToMaybe . C.destroyCookieJar <$> atomically (readTVar cj)
      C.cookie_name <$> cookie `shouldBe` Just "testcookie"
      C.cookie_value <$> cookie `shouldBe` Just "test"

    it "Can modify the outgoing Request using the ClientEnv" $ \(_, baseUrl) -> do
      mgr <- C.newManager C.defaultManagerSettings
      -- In proper situation, extra headers should probably be visible in API type.
      -- However, testing for response timeout is difficult, so we test with something which is easy to observe
      let createClientRequest url r = (defaultMakeClientRequest url r) { C.requestHeaders = [("X-Added-Header", "XXX")] }
      let clientEnv = (mkClientEnv mgr baseUrl) { makeClientRequest = createClientRequest }
      res <- runClientM (getRawSuccessPassHeaders HTTP.methodGet) clientEnv
      case res of
        Left e ->
          assertFailure $ show e
        Right r ->
          ("X-Added-Header", "XXX") `elem` toList (responseHeaders r) `shouldBe` True

    modifyMaxSuccess (const 20) $ do
      it "works for a combination of Capture, QueryParam, QueryFlag and ReqBody" $ \(_, baseUrl) ->
        property $ forAllShrink pathGen shrink $ \(NonEmpty cap) num flag body ->
          ioProperty $ do
            result <- left show <$> runClient (getMultiple cap num flag body) baseUrl
            return $
              result === Right (cap, num, flag, body)
