{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | These are custom combinators for Servant.Server.UsingConfigSpec.
--
-- (For writing your own combinators you need to import Internal modules, for
-- just *using* combinators that require a Config, you don't. This module is
-- separate from Servant.Server.UsingConfigSpec to test that the module imports
-- work out this way.)
module Servant.Server.UsingConfigSpec.TestCombinators where

import           GHC.TypeLits

import           Servant
import           Servant.Server.Internal.Config
import           Servant.Server.Internal.RoutingApplication

data ExtractFromConfig

instance (HasServer subApi) =>
  HasServer (ExtractFromConfig :> subApi) where

  type ServerT (ExtractFromConfig :> subApi) m =
    String -> ServerT subApi m
  type HasConfig (ExtractFromConfig :> subApi) (c :: [*]) =
    (HasConfigEntry c String, HasConfig subApi c)

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
  type HasConfig (InjectIntoConfig :> subApi) c =
    (HasConfig subApi (String ': c))

  route Proxy config delayed =
    route subProxy newConfig delayed
    where
      subProxy :: Proxy subApi
      subProxy = Proxy

      newConfig = ("injected" :: String) :. config

data NamedConfigWithBirdface (name :: Symbol) (subConfig :: [*])

instance (HasServer subApi) =>
  HasServer (NamedConfigWithBirdface name subConfig :> subApi) where

  type ServerT (NamedConfigWithBirdface name subConfig :> subApi) m =
    ServerT subApi m
  type HasConfig (NamedConfigWithBirdface name subConfig :> subApi) config =
    (HasConfigEntry config (NamedConfig name subConfig), HasConfig subApi subConfig)

  route Proxy config delayed =
    route subProxy subConfig delayed
    where
      subProxy :: Proxy subApi
      subProxy = Proxy

      subConfig :: Config subConfig
      subConfig = descendIntoNamedConfig (Proxy :: Proxy name) config
