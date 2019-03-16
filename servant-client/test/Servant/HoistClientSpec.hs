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

module Servant.HoistClientSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Data.Monoid ()
import           Data.Proxy
import           Test.Hspec

import           Servant.API
                 ((:<|>) ((:<|>)), (:>), Capture,
                 Get, JSON, Post)
import           Servant.Client
import           Servant.Server
import           Servant.ClientTestUtils


spec :: Spec
spec = describe "Servant.HoistClientSpec" $ do
    hoistClientSpec

type HoistClientAPI = Get '[JSON] Int :<|> Capture "n" Int :> Post '[JSON] Int

hoistClientAPI :: Proxy HoistClientAPI
hoistClientAPI = Proxy

hoistClientServer :: Application -- implements HoistClientAPI
hoistClientServer = serve hoistClientAPI $ return 5 :<|> return 

hoistClientSpec :: Spec
hoistClientSpec = beforeAll (startWaiApp hoistClientServer) $ afterAll endWaiApp $ do
  describe "Servant.Client.hoistClient" $ do
    it "allows us to GET/POST/... requests in IO instead of ClientM" $ \(_, baseUrl) -> do
      let (getInt :<|> postInt)
            = hoistClient hoistClientAPI
                          (fmap (either (error . show) id) . flip runClient baseUrl)
                          (client hoistClientAPI)

      getInt `shouldReturn` 5
      postInt 5 `shouldReturn` 5
