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

type OneEntryAPI =
  CustomCombinator () :> Get '[JSON] String

testServer :: Server OneEntryAPI
testServer s = return s

oneEntryApp :: Application
oneEntryApp =
  serve (Proxy :: Proxy OneEntryAPI) config testServer
  where
    config = 'a' :. EmptyConfig

type OneEntryTwiceAPI =
  "foo" :> CustomCombinator () :> Get '[JSON] String :<|>
  "bar" :> CustomCombinator () :> Get '[JSON] String

oneEntryTwiceApp :: Application
oneEntryTwiceApp = serve (Proxy :: Proxy OneEntryTwiceAPI) config $
  testServer :<|>
  testServer
  where
    config = '2' :. EmptyConfig

type TwoDifferentEntries =
  "foo" :> CustomCombinator "foo" :> Get '[JSON] String :<|>
  "bar" :> CustomCombinator "bar" :> Get '[JSON] String

twoDifferentEntries :: Application
twoDifferentEntries = serve (Proxy :: Proxy TwoDifferentEntries) config $
  testServer :<|>
  testServer
  where
    config =
      (Tag 'x' :: Tagged "foo" Char) :.
      (Tag 'y' :: Tagged "bar" Char) :.
      EmptyConfig

-- * tests

spec :: Spec
spec = do
  describe "using Config in a custom combinator" $ do
    with (return oneEntryApp) $ do
      it "allows to retrieve a ConfigEntry" $ do
        get "/" `shouldRespondWith` "\"a\""

    with (return oneEntryTwiceApp) $ do
      it "allows to retrieve the same ConfigEntry twice" $ do
        get "/foo" `shouldRespondWith` "\"2\""
        get "/bar" `shouldRespondWith` "\"2\""

    with (return twoDifferentEntries) $ do
      it "allows to retrieve different ConfigEntries for the same combinator" $ do
        get "/foo" `shouldRespondWith` "\"x\""
        get "/bar" `shouldRespondWith` "\"y\""
