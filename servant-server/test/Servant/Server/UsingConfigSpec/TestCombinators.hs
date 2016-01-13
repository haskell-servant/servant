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

import           GHC.TypeLits

import           Servant
import           Servant.Server.Internal.Config
import           Servant.Server.Internal.RoutingApplication

data ExtractFromConfig

instance forall subApi (c :: [*]) .
  (HasServer subApi) =>
  HasServer (ExtractFromConfig :> subApi) where

  type ServerT (ExtractFromConfig :> subApi) m =
    String -> ServerT subApi m
  type HasCfg (ExtractFromConfig :> subApi) c =
    (HasConfigEntry c String, HasCfg subApi c)

  route Proxy config delayed =
    route subProxy config (fmap (inject config) delayed :: Delayed (Server subApi))
    where
      subProxy :: Proxy subApi
      subProxy = Proxy

      inject config f = f (getConfigEntry config)

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

      newConfig = ("injected" :: String) :. config

data SubConfig (name :: Symbol) (subConfig :: [*])

newtype Tagged tag a = Tag a
  deriving (Show, Eq)

instance (HasServer subApi) =>
  HasServer (SubConfig name subConfig :> subApi) where

  type ServerT (SubConfig name subConfig :> subApi) m =
    ServerT subApi m
  type HasCfg (SubConfig name subConfig :> subApi) config =
    (HasConfigEntry config (Tagged name (Config subConfig)), HasCfg subApi subConfig)

  route Proxy config delayed =
    route subProxy subConfig delayed
    where
      subProxy :: Proxy subApi
      subProxy = Proxy

      subConfig :: Config subConfig
      subConfig =
        let Tag x = getConfigEntry config :: Tagged name (Config subConfig)
        in x
