{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Server.UsingConfigSpec where

import           Network.Wai
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.Wai

import           Servant
import           Servant.Server.UsingConfigSpec.TestCombinators

-- * API

type OneEntryAPI =
  ExtractFromConfig () :> Get '[JSON] String

testServer :: Server OneEntryAPI
testServer s = return s

oneEntryApp :: Application
oneEntryApp =
  serve (Proxy :: Proxy OneEntryAPI) config testServer
  where
    config = ("configEntry" :: String) :. EmptyConfig

type OneEntryTwiceAPI =
  "foo" :> ExtractFromConfig () :> Get '[JSON] String :<|>
  "bar" :> ExtractFromConfig () :> Get '[JSON] String

oneEntryTwiceApp :: Application
oneEntryTwiceApp = serve (Proxy :: Proxy OneEntryTwiceAPI) config $
  testServer :<|>
  testServer
  where
    config = ("configEntryTwice" :: String) :. EmptyConfig

type TwoDifferentEntries =
  "foo" :> ExtractFromConfig "foo" :> Get '[JSON] String :<|>
  "bar" :> ExtractFromConfig "bar" :> Get '[JSON] String

twoDifferentEntries :: Application
twoDifferentEntries = serve (Proxy :: Proxy TwoDifferentEntries) config $
  testServer :<|>
  testServer
  where
    config =
      (Tag "firstEntry" :: Tagged "foo" String) :.
      (Tag "secondEntry" :: Tagged "bar" String) :.
      EmptyConfig

-- * tests

spec :: Spec
spec = do
  describe "accessing config entries from custom combinators" $ do
    with (return oneEntryApp) $ do
      it "allows to retrieve a ConfigEntry" $ do
        get "/" `shouldRespondWith` "\"configEntry\""

    with (return oneEntryTwiceApp) $ do
      it "allows to retrieve the same ConfigEntry twice" $ do
        get "/foo" `shouldRespondWith` "\"configEntryTwice\""
        get "/bar" `shouldRespondWith` "\"configEntryTwice\""

    with (return twoDifferentEntries) $ do
      it "allows to retrieve different ConfigEntries for the same combinator" $ do
        get "/foo" `shouldRespondWith` "\"firstEntry\""
        get "/bar" `shouldRespondWith` "\"secondEntry\""

  spec2

type InjectAPI =
  InjectIntoConfig () :> "untagged" :> ExtractFromConfig () :>
    Get '[JSON] String :<|>
  InjectIntoConfig "tag" :> "tagged" :> ExtractFromConfig "tag" :>
    Get '[JSON] String

injectApp :: Application
injectApp = serve (Proxy :: Proxy InjectAPI) config $
  (\ s -> return s) :<|>
  (\ s -> return ("tagged: " ++ s))
  where
    config = EmptyConfig

spec2 :: Spec
spec2 = do
  with (return injectApp) $ do
    describe "inserting config entries with custom combinators" $ do
      it "allows to inject config entries" $ do
        get "/untagged" `shouldRespondWith` "\"injected\""

      it "allows to inject tagged config entries" $ do
        get "/tagged" `shouldRespondWith` "\"tagged: injected\""
