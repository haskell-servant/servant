{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Server.UsingConfigSpec where

import           Control.Monad.Trans.Except
import           Network.Wai
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.Wai

import           Servant
import           Servant.Server.UsingConfigSpec.TestCombinators

spec :: Spec
spec = do
  spec1
  spec2
  spec3
  spec4

-- * API

type OneEntryAPI =
  ExtractFromConfig :> Get '[JSON] String

testServer :: String -> ExceptT ServantErr IO String
testServer s = return s

oneEntryApp :: Application
oneEntryApp =
  serve (Proxy :: Proxy OneEntryAPI) config testServer
  where
    config :: Config '[String]
    config = "configEntry" :. EmptyConfig

type OneEntryTwiceAPI =
  "foo" :> ExtractFromConfig :> Get '[JSON] String :<|>
  "bar" :> ExtractFromConfig :> Get '[JSON] String

oneEntryTwiceApp :: Application
oneEntryTwiceApp = serve (Proxy :: Proxy OneEntryTwiceAPI) config $
  testServer :<|>
  testServer
  where
    config :: Config '[String]
    config = "configEntryTwice" :. EmptyConfig

-- * tests

spec1 :: Spec
spec1 = do
  describe "accessing config entries from custom combinators" $ do
    with (return oneEntryApp) $ do
      it "allows retrieving a ConfigEntry" $ do
        get "/" `shouldRespondWith` "\"configEntry\""

    with (return oneEntryTwiceApp) $ do
      it "allows retrieving the same ConfigEntry twice" $ do
        get "/foo" `shouldRespondWith` "\"configEntryTwice\""
        get "/bar" `shouldRespondWith` "\"configEntryTwice\""

type InjectAPI =
  InjectIntoConfig :> "untagged" :> ExtractFromConfig :>
    Get '[JSON] String :<|>
  InjectIntoConfig :> "tagged" :> ExtractFromConfig :>
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

type WithBirdfaceAPI =
  "foo" :> ExtractFromConfig :> Get '[JSON] String :<|>
  NamedConfigWithBirdface "sub" '[String] :>
    "bar" :> ExtractFromConfig :> Get '[JSON] String

withBirdfaceApp :: Application
withBirdfaceApp = serve (Proxy :: Proxy WithBirdfaceAPI) config $
  testServer :<|>
  testServer
  where
    config :: Config '[String, (NamedConfig "sub" '[String])]
    config =
      "firstEntry" :.
      (NamedConfig ("secondEntry" :. EmptyConfig)) :.
      EmptyConfig

spec3 :: Spec
spec3 = do
  with (return withBirdfaceApp) $ do
    it "allows retrieving different ConfigEntries for the same combinator" $ do
      get "/foo" `shouldRespondWith` "\"firstEntry\""
      get "/bar" `shouldRespondWith` "\"secondEntry\""

type NamedConfigAPI =
  WithNamedConfig "sub" '[String] (
    ExtractFromConfig :> Get '[JSON] String)

namedConfigApp :: Application
namedConfigApp = serve (Proxy :: Proxy NamedConfigAPI) config return
  where
    config :: Config '[NamedConfig "sub" '[String]]
    config = NamedConfig ("descend" :. EmptyConfig) :. EmptyConfig

spec4 :: Spec
spec4 = do
  with (return namedConfigApp) $ do
    describe "WithNamedConfig" $ do
      it "allows descending into a subconfig for a given api" $ do
        get "/" `shouldRespondWith` "\"descend\""
