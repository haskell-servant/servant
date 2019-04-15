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

module Servant.BasicAuthSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Control.Arrow
                 (left)
import           Data.Monoid ()
import qualified Network.HTTP.Types                   as HTTP
import           Test.Hspec

import           Servant.API
                 (BasicAuthData (..))
import           Servant.Client
import           Servant.ClientTestUtils

spec :: Spec
spec = describe "Servant.BasicAuthSpec" $ do
    basicAuthSpec

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
