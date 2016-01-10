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

import           Servant
import           Servant.Server.Internal.Config
import           Servant.Server.Internal.RoutingApplication

data CustomCombinator (entryType :: *)

class ToCustomConfig entryType where
  toCustomConfig :: entryType -> String

instance ToCustomConfig String where
  toCustomConfig = id

instance forall subApi (c :: [*]) entryType .
  (HasServer subApi, ToCustomConfig entryType) =>
  HasServer (CustomCombinator entryType :> subApi) where

  type ServerT (CustomCombinator entryType :> subApi) m =
    String -> ServerT subApi m
  type HasCfg (CustomCombinator entryType :> subApi) c =
    (HasConfigEntry c entryType, HasCfg subApi c)

  route Proxy config delayed =
    route subProxy config (fmap (inject config) delayed :: Delayed (Server subApi))
    where
      subProxy :: Proxy subApi
      subProxy = Proxy

      inject config f = f (toCustomConfig (getConfigEntry config :: entryType))
