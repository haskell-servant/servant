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

spec :: Spec
spec = do
  describe "accessing config entries from custom combinators" $ do
    with (return oneEntryApp) $ do
      it "allows retrieving a ConfigEntry" $ do
        get "/" `shouldRespondWith` "\"configEntry\""

    with (return oneEntryTwiceApp) $ do
      it "allows retrieving the same ConfigEntry twice" $ do
        get "/foo" `shouldRespondWith` "\"configEntryTwice\""
        get "/bar" `shouldRespondWith` "\"configEntryTwice\""

  spec2

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

  spec3

type SubConfigAPI =
  "foo" :> ExtractFromConfig :> Get '[JSON] String :<|>
  SubConfig "sub" '[String] :>
    "bar" :> ExtractFromConfig :> Get '[JSON] String

subConfigApp :: Application
subConfigApp = serve (Proxy :: Proxy SubConfigAPI) config $
  testServer :<|>
  testServer
  where
    config :: Config '[String, (SubConfig "sub" '[String])]
    config =
      "firstEntry" :.
      (SubConfig ("secondEntry" :. EmptyConfig)) :.
      EmptyConfig

spec3 :: Spec
spec3 = do
  with (return subConfigApp) $ do
    it "allows retrieving different ConfigEntries for the same combinator" $ do
      get "/foo" `shouldRespondWith` "\"firstEntry\""
      get "/bar" `shouldRespondWith` "\"secondEntry\""
