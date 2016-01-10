{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Server.UsingConfigSpec where

import           Network.Wai
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.Wai

import           Servant
import           Servant.Server.UsingConfigSpec.CustomCombinator

-- * API

type API =
  CustomCombinator :> Get '[JSON] String

api :: Proxy API
api = Proxy

testServer :: Server API
testServer (CustomConfig s) = return s

app :: Application
app =
  serve api config testServer
  where
    config :: Config '[ConfigEntry Tag CustomConfig]
    config = CustomConfig "configValue" .:. EmptyConfig

-- * tests

spec :: Spec
spec = do
  describe "using Config in a custom combinator" $ do
    with (return app) $ do
      it "allows to retrieve the ConfigEntry" $ do
        get "/" `shouldRespondWith` "\"configValue\""
