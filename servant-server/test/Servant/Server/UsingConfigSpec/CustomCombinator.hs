{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | This is a custom combinator for S.S.UsingConfigSpec. It's split up into
-- its own module to be able to test how exactly module import work when using
-- the config.
module Servant.Server.UsingConfigSpec.CustomCombinator where

import           Servant
import           Servant.Server.Internal.Config
import           Servant.Server.Internal.RoutingApplication

data CustomCombinator (tag :: k)

instance forall subApi (c :: [*]) tag .
  (HasServer subApi) =>
  HasServer (CustomCombinator tag :> subApi) where

  type ServerT (CustomCombinator tag :> subApi) m =
    String -> ServerT subApi m
  type HasCfg (CustomCombinator tag :> subApi) c =
    (HasConfigEntry c tag Char, HasCfg subApi c)

  route Proxy config delayed =
    route subProxy config (fmap (inject config) delayed :: Delayed (Server subApi))
    where
      subProxy :: Proxy subApi
      subProxy = Proxy

      inject config f = f [getConfigEntry (Proxy :: Proxy tag) config]
