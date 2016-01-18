{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

instance (HasConfigEntry config String, HasServer subApi config) =>
  HasServer (ExtractFromConfig :> subApi) config where

  type ServerT (ExtractFromConfig :> subApi) m =
    String -> ServerT subApi m

  route Proxy config delayed =
    route subProxy config (fmap (inject config) delayed :: Delayed (Server subApi))
    where
      subProxy :: Proxy subApi
      subProxy = Proxy

      inject config f = f (getConfigEntry config)

data InjectIntoConfig

instance (HasServer subApi (String ': config)) =>
  HasServer (InjectIntoConfig :> subApi) config where

  type ServerT (InjectIntoConfig :> subApi) m =
    ServerT subApi m

  route Proxy config delayed =
    route subProxy newConfig delayed
    where
      subProxy :: Proxy subApi
      subProxy = Proxy

      newConfig = ("injected" :: String) :. config

data NamedConfigWithBirdface (name :: Symbol) (subConfig :: [*])

instance (HasConfigEntry config (NamedConfig name subConfig), HasServer subApi subConfig) =>
  HasServer (NamedConfigWithBirdface name subConfig :> subApi) config where

  type ServerT (NamedConfigWithBirdface name subConfig :> subApi) m =
    ServerT subApi m

  route Proxy config delayed =
    route subProxy subConfig delayed
    where
      subProxy :: Proxy subApi
      subProxy = Proxy

      subConfig :: Config subConfig
      subConfig = descendIntoNamedConfig (Proxy :: Proxy name) config
