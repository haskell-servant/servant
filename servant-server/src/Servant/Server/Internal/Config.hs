{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

#include "overlapping-compat.h"

module Servant.Server.Internal.Config where

import Control.DeepSeq (NFData(rnf))
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

-- | The entire configuration.
data Config a where
    EmptyConfig :: Config '[]
    ConsConfig :: x -> Config xs -> Config (x ': xs)

instance Show (Config '[]) where
  show EmptyConfig = "EmptyConfig"
instance (Show a, Show (Config as)) => Show (Config (a ': as)) where
  showsPrec outerPrecedence (ConsConfig a as) =
    showParen (outerPrecedence > 5) $
      shows a . showString " .:. " . shows as

instance Eq (Config '[]) where
    _ == _ = True
instance (Eq a, Eq (Config as)) => Eq (Config (a ': as)) where
    ConsConfig x1 y1 == ConsConfig x2 y2 = x1 == x2 && y1 == y2

(.:.) :: x -> Config xs -> Config (x ': xs)
e .:. cfg = ConsConfig e cfg
infixr 5 .:.

class HasConfigEntry (cfg :: [*]) (val :: *) where
    getConfigEntry :: Config cfg -> val

instance OVERLAPPABLE_
         HasConfigEntry xs val => HasConfigEntry (notIt ': xs) val where
    getConfigEntry (ConsConfig _ xs) = getConfigEntry xs

instance OVERLAPPABLE_
         HasConfigEntry (val ': xs) val where
    getConfigEntry (ConsConfig x _) = x
