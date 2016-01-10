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

data Tag1
data Tag2

type OneEntryAPI =
  CustomCombinator Tag1 :> Get '[JSON] String

testServer :: Server OneEntryAPI
testServer (CustomConfig s) = return s

oneEntryApp :: Application
oneEntryApp =
  serve (Proxy :: Proxy OneEntryAPI) config testServer
  where
    config :: Config '[ConfigEntry Tag1 CustomConfig]
    config = CustomConfig "configValue" .:. EmptyConfig

type OneEntryTwiceAPI =
  "foo" :> CustomCombinator Tag1 :> Get '[JSON] String :<|>
  "bar" :> CustomCombinator Tag1 :> Get '[JSON] String

oneEntryTwiceApp :: Application
oneEntryTwiceApp = serve (Proxy :: Proxy OneEntryTwiceAPI) config $
  testServer :<|>
  testServer
  where
    config :: Config '[ConfigEntry Tag1 CustomConfig]
    config = CustomConfig "configValueTwice" .:. EmptyConfig

type TwoDifferentEntries =
  "foo" :> CustomCombinator Tag1 :> Get '[JSON] String :<|>
  "bar" :> CustomCombinator Tag2 :> Get '[JSON] String

twoDifferentEntries :: Application
twoDifferentEntries = serve (Proxy :: Proxy TwoDifferentEntries) config $
  testServer :<|>
  testServer
  where
    config :: Config '[ConfigEntry Tag1 CustomConfig, ConfigEntry Tag2 CustomConfig]
    config =
      CustomConfig "firstConfigValue" .:.
      CustomConfig "secondConfigValue" .:.
      EmptyConfig

-- * tests

spec :: Spec
spec = do
  describe "using Config in a custom combinator" $ do
    with (return oneEntryApp) $ do
      it "allows to retrieve a ConfigEntry" $ do
        get "/" `shouldRespondWith` "\"configValue\""

    with (return oneEntryTwiceApp) $ do
      it "allows to retrieve the same ConfigEntry twice" $ do
        get "/foo" `shouldRespondWith` "\"configValueTwice\""
        get "/bar" `shouldRespondWith` "\"configValueTwice\""

    with (return twoDifferentEntries) $ do
      it "allows to retrieve different ConfigEntries for the same combinator" $ do
        get "/foo" `shouldRespondWith` "\"firstConfigValue\""
        get "/bar" `shouldRespondWith` "\"secondConfigValue\""
