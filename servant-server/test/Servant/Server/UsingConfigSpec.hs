{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Server.UsingConfigSpec where

import           Network.Wai
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.Wai

import           Servant
import           Servant.Server.UsingConfigSpec.CustomCombinator

-- * API

newtype Wrapped a = Wrap { unwrap :: a }

instance ToCustomConfig (Wrapped CustomConfig) where
  toCustomConfig = unwrap

type OneEntryAPI =
  CustomCombinator CustomConfig :> Get '[JSON] String

testServer :: Server OneEntryAPI
testServer (CustomConfig s) = return s

oneEntryApp :: Application
oneEntryApp =
  serve (Proxy :: Proxy OneEntryAPI) config testServer
  where
    config :: Config '[CustomConfig]
    config = CustomConfig "configValue" .:. EmptyConfig

type OneEntryTwiceAPI =
  "foo" :> CustomCombinator CustomConfig :> Get '[JSON] String :<|>
  "bar" :> CustomCombinator CustomConfig :> Get '[JSON] String

oneEntryTwiceApp :: Application
oneEntryTwiceApp = serve (Proxy :: Proxy OneEntryTwiceAPI) config $
  testServer :<|>
  testServer
  where
    config :: Config '[CustomConfig]
    config = CustomConfig "configValueTwice" .:. EmptyConfig

type TwoDifferentEntries =
  "foo" :> CustomCombinator CustomConfig :> Get '[JSON] String :<|>
  "bar" :> CustomCombinator (Wrapped CustomConfig) :> Get '[JSON] String

twoDifferentEntries :: Application
twoDifferentEntries = serve (Proxy :: Proxy TwoDifferentEntries) config $
  testServer :<|>
  testServer
  where
    config :: Config '[CustomConfig, Wrapped CustomConfig]
    config =
      CustomConfig "firstConfigValue" .:.
      Wrap (CustomConfig "secondConfigValue") .:.
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
