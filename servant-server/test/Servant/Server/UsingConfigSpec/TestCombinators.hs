{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This is a custom combinator for S.S.UsingConfigSpec. It's split up into
-- its own module to be able to test how exactly module import work when using
-- the config.
module Servant.Server.UsingConfigSpec.TestCombinators where

import           Servant
import           Servant.Server.Internal.Config
import           Servant.Server.Internal.RoutingApplication

data ExtractFromConfig (tag :: k)

instance forall subApi (c :: [*]) tag .
  (HasServer subApi) =>
  HasServer (ExtractFromConfig tag :> subApi) where

  type ServerT (ExtractFromConfig tag :> subApi) m =
    String -> ServerT subApi m
  type HasCfg (ExtractFromConfig tag :> subApi) c =
    (HasConfigEntry c tag String, HasCfg subApi c)

  route Proxy config delayed =
    route subProxy config (fmap (inject config) delayed :: Delayed (Server subApi))
    where
      subProxy :: Proxy subApi
      subProxy = Proxy

      inject config f = f (getConfigEntry (Proxy :: Proxy tag) config)

data InjectIntoConfig

instance (HasServer subApi) =>
  HasServer (InjectIntoConfig :> subApi) where

  type ServerT (InjectIntoConfig :> subApi) m =
    ServerT subApi m
  type HasCfg (InjectIntoConfig :> subApi) c =
    (HasCfg subApi (String ': c))

  route Proxy config delayed =
    route subProxy newConfig delayed
    where
      subProxy :: Proxy subApi
      subProxy = Proxy

      newConfig = "injected" :. config
