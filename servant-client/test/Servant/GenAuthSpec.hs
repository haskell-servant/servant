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

module Servant.GenAuthSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Control.Arrow
                 (left)
import           Data.Monoid ()
import qualified Network.HTTP.Types                   as HTTP
import           Test.Hspec

import           Servant.Client
import qualified Servant.Client.Core.Auth    as Auth
import qualified Servant.Client.Core.Request as Req
import           Servant.ClientTestUtils

spec :: Spec
spec = describe "Servant.GenAuthSpec" $ do
    genAuthSpec

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
      responseStatusCode r `shouldBe` HTTP.Status 401 "Unauthorized"

