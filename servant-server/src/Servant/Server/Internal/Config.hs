{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

#include "overlapping-compat.h"

module Servant.Server.Internal.Config where

import           Data.Proxy
import           GHC.TypeLits

-- | The entire configuration.
data Config a where
    EmptyConfig :: Config '[]
    (:.) :: x -> Config xs -> Config (x ': xs)
infixr 5 :.

instance Show (Config '[]) where
  show EmptyConfig = "EmptyConfig"
instance (Show a, Show (Config as)) => Show (Config (a ': as)) where
  showsPrec outerPrecedence (a :. as) =
    showParen (outerPrecedence > 5) $
      shows a . showString " :. " . shows as

instance Eq (Config '[]) where
    _ == _ = True
instance (Eq a, Eq (Config as)) => Eq (Config (a ': as)) where
    x1 :. y1 == x2 :. y2 = x1 == x2 && y1 == y2

class HasConfigEntry (config :: [*]) (val :: *) where
    getConfigEntry :: Config config -> val

instance OVERLAPPABLE_
         HasConfigEntry xs val => HasConfigEntry (notIt ': xs) val where
    getConfigEntry (_ :. xs) = getConfigEntry xs

instance OVERLAPPING_
         HasConfigEntry (val ': xs) val where
    getConfigEntry (x :. _) = x

-- * support for named subconfigs

data NamedConfig (name :: Symbol) (subConfig :: [*])
  = NamedConfig (Config subConfig)

descendIntoNamedConfig :: forall config name subConfig .
  HasConfigEntry config (NamedConfig name subConfig) =>
  Proxy (name :: Symbol) -> Config config -> Config subConfig
descendIntoNamedConfig Proxy config =
  let NamedConfig subConfig = getConfigEntry config :: NamedConfig name subConfig
  in subConfig
