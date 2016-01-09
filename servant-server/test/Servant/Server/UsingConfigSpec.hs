{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Server.UsingConfigSpec where

import           Network.Wai
import           Test.Hspec (Spec, describe, it)
import           Test.Hspec.Wai

import           Servant
import           Servant.Server.Internal.Config
import           Servant.Server.Internal.Router
import           Servant.Server.Internal.RoutingApplication

-- * custom test combinator

data CustomCombinator

data CustomConfig = CustomConfig String

data Tag

instance forall subApi (c :: [*]) .
  (HasServer subApi) =>
  HasServer (CustomCombinator :> subApi) where

  type ServerT (CustomCombinator :> subApi) m =
    CustomConfig -> ServerT subApi m
  type HasCfg (CustomCombinator :> subApi) c =
    (HasConfigEntry c Tag CustomConfig, HasCfg subApi c)

  route Proxy config delayed =
    route subProxy config (fmap (inject config) delayed :: Delayed (Server subApi))
    where
      subProxy :: Proxy subApi
      subProxy = Proxy

      inject config f = f (getConfigEntry (Proxy :: Proxy Tag) config)

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
    config = CustomConfig "configValue" .: EmptyConfig

-- * tests

spec :: Spec
spec = do
  describe "using Config in a custom combinator" $ do
    with (return app) $ do
      it "allows to retrieve the ConfigEntry" $ do
        get "/" `shouldRespondWith` "\"configValue\""
