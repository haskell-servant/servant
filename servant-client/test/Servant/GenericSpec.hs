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

module Servant.GenericSpec (spec) where

import           Test.Hspec

import           Servant.Client ((//), (/:))
import           Servant.ClientTestUtils

spec :: Spec
spec = describe "Servant.GenericSpec" $ do
    genericSpec

genericSpec :: Spec
genericSpec = beforeAll (startWaiApp server) $ afterAll endWaiApp $ do
  context "Record clients work as expected" $ do

    it "Client functions return expected values" $ \(_,baseUrl) -> do
      runClient (recordRoutes // version) baseUrl `shouldReturn` Right 42
      runClient (recordRoutes // echo /: "foo") baseUrl `shouldReturn` Right "foo"
    it "Clients can be nested" $ \(_,baseUrl) -> do
      runClient (recordRoutes // otherRoutes /: 42 // something) baseUrl `shouldReturn` Right ["foo", "bar", "pweet"]
