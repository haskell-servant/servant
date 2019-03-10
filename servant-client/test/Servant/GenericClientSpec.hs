{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -freduction-depth=100 #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Servant.GenericClientSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Data.Proxy
import           Test.Hspec

import           Control.Arrow
                 (left)
import           Servant.Client

import           Servant.TestUtils

spec :: Spec
spec = describe "Servant.GenericClientSpec" $ do
  genericClientSpec

genericClientSpec :: Spec
genericClientSpec = beforeAll (startWaiApp genericClientServer) $ afterAll endWaiApp $ do
  describe "Servant.Client.Generic" $ do

    let GenericClient{..} = mkClient (client (Proxy :: Proxy GenericClientAPI))
        NestedClient1{..} = mkNestedClient1 "example"
        NestedClient2{..} = mkNestedClient2 (Just 42)

    it "works for top-level client inClientM function" $ \(_, baseUrl) -> do
      left show <$> runClient (getSqr (Just 5)) baseUrl `shouldReturn` Right 25

    it "works for nested clients" $ \(_, baseUrl) -> do
      left show <$> runClient (idChar (Just 'c')) baseUrl `shouldReturn` Right 'c'
      left show <$> runClient (getSum 3 4) baseUrl `shouldReturn` Right 7
      left show <$> runClient doNothing baseUrl `shouldReturn` Right ()

