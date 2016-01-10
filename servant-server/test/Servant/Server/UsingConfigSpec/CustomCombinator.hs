{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{- LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{- LANGUAGE UndecidableInstances #-}

-- | This is a custom combinator for S.S.UsingConfigSpec. It's split up into
-- its own module to be able to test how exactly module import work when using
-- the config.
module Servant.Server.UsingConfigSpec.CustomCombinator where

-- import           Network.Wai
-- import           Test.Hspec (Spec, describe, it)
-- import           Test.Hspec.Wai

import           Servant
import           Servant.Server.Internal.Config
-- import           Servant.Server.Internal.Router
import           Servant.Server.Internal.RoutingApplication

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

