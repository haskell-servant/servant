{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Servant.Server.UsingContextSpec where

import           Network.Wai
import           Test.Hspec
                 (Spec, describe, it)
import           Test.Hspec.Wai

import           Servant
import           Servant.Server.UsingContextSpec.TestCombinators

spec :: Spec
spec = do
  spec1
  spec2
  spec3
  spec4

-- * API

type OneEntryAPI =
  ExtractFromContext :> Get '[JSON] String

testServer :: String -> Handler String
testServer s = return s

oneEntryApp :: Application
oneEntryApp =
  serveWithContext (Proxy :: Proxy OneEntryAPI) context testServer
  where
    context :: Context '[String]
    context = "contextEntry" :. EmptyContext

type OneEntryTwiceAPI =
  "foo" :> ExtractFromContext :> Get '[JSON] String :<|>
  "bar" :> ExtractFromContext :> Get '[JSON] String

oneEntryTwiceApp :: Application
oneEntryTwiceApp = serveWithContext (Proxy :: Proxy OneEntryTwiceAPI) context $
  testServer :<|>
  testServer
  where
    context :: Context '[String]
    context = "contextEntryTwice" :. EmptyContext

-- * tests

spec1 :: Spec
spec1 = do
  describe "accessing context entries from custom combinators" $ do
    with (return oneEntryApp) $ do
      it "allows retrieving a ContextEntry" $ do
        get "/" `shouldRespondWith` "\"contextEntry\""

    with (return oneEntryTwiceApp) $ do
      it "allows retrieving the same ContextEntry twice" $ do
        get "/foo" `shouldRespondWith` "\"contextEntryTwice\""
        get "/bar" `shouldRespondWith` "\"contextEntryTwice\""

type InjectAPI =
  InjectIntoContext :> "untagged" :> ExtractFromContext :>
    Get '[JSON] String :<|>
  InjectIntoContext :> "tagged" :> ExtractFromContext :>
    Get '[JSON] String

injectApp :: Application
injectApp = serveWithContext (Proxy :: Proxy InjectAPI) context $
  (\ s -> return s) :<|>
  (\ s -> return ("tagged: " ++ s))
  where
    context = EmptyContext

spec2 :: Spec
spec2 = do
  with (return injectApp) $ do
    describe "inserting context entries with custom combinators" $ do
      it "allows to inject context entries" $ do
        get "/untagged" `shouldRespondWith` "\"injected\""

      it "allows to inject tagged context entries" $ do
        get "/tagged" `shouldRespondWith` "\"tagged: injected\""

type WithBirdfaceAPI =
  "foo" :> ExtractFromContext :> Get '[JSON] String :<|>
  NamedContextWithBirdface "sub" '[String] :>
    "bar" :> ExtractFromContext :> Get '[JSON] String

withBirdfaceApp :: Application
withBirdfaceApp = serveWithContext (Proxy :: Proxy WithBirdfaceAPI) context $
  testServer :<|>
  testServer
  where
    context :: Context '[String, (NamedContext "sub" '[String])]
    context =
      "firstEntry" :.
      (NamedContext ("secondEntry" :. EmptyContext)) :.
      EmptyContext

spec3 :: Spec
spec3 = do
  with (return withBirdfaceApp) $ do
    it "allows retrieving different ContextEntries for the same combinator" $ do
      get "/foo" `shouldRespondWith` "\"firstEntry\""
      get "/bar" `shouldRespondWith` "\"secondEntry\""

type NamedContextAPI =
  WithNamedContext "sub" '[String] (
    ExtractFromContext :> Get '[JSON] String)

namedContextApp :: Application
namedContextApp = serveWithContext (Proxy :: Proxy NamedContextAPI) context return
  where
    context :: Context '[NamedContext "sub" '[String]]
    context = NamedContext ("descend" :. EmptyContext) :. EmptyContext

spec4 :: Spec
spec4 = do
  with (return namedContextApp) $ do
    describe "WithNamedContext" $ do
      it "allows descending into a subcontext for a given api" $ do
        get "/" `shouldRespondWith` "\"descend\""
