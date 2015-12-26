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
#if !MIN_VERSION_base(4,8,0)
{-# LANGUAGE OverlappingInstances       #-}
#endif
module Servant.Server.Internal.Config where

import Control.DeepSeq (NFData(rnf))
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

-- | A single entry in the configuration. The first parameter is phantom, and
-- is used to lookup a @ConfigEntry@ in a @Config@.
newtype ConfigEntry tag a = ConfigEntry { unConfigEntry :: a }
  deriving ( Eq, Show, Read, Enum, Integral, Fractional, Generic, Typeable
           , Num, Ord, Real, Functor, Foldable, Traversable, NFData)

instance Applicative (ConfigEntry tag) where
    pure = ConfigEntry
    ConfigEntry f <*> ConfigEntry a = ConfigEntry $ f a

instance Monad (ConfigEntry tag) where
    return = ConfigEntry
    ConfigEntry a >>= f = f a

-- | The entire configuration.
data Config a where
    EmptyConfig :: Config '[]
    ConsConfig :: x -> Config xs -> Config (x ': xs)

instance Eq (Config '[]) where
    _ == _ = True
instance (Eq a, Eq (Config as)) => Eq (Config (a ' : as)) where
    ConsConfig x1 y1 == ConsConfig x2 y2 = x1 == x2 && y1 == y2

instance NFData (Config '[]) where
    rnf EmptyConfig = ()
instance (NFData a, NFData (Config as)) => NFData (Config (a ': as)) where
    rnf (x `ConsConfig` ys) = rnf x `seq` rnf ys



(.:) :: x -> Config xs -> Config (ConfigEntry tag x ': xs)
e .: cfg = ConsConfig (ConfigEntry e) cfg
infixr 4 .:

class HasConfigEntry (cfg :: [*]) (a :: k) (val :: *) | cfg a -> val where
    getConfigEntry :: proxy a -> Config cfg -> val

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPABLE #-}
#endif
         HasConfigEntry xs tag val => HasConfigEntry (notIt ': xs) tag val where
    getConfigEntry p (ConsConfig _ xs) = getConfigEntry p xs

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPABLE #-}
#endif
         HasConfigEntry (ConfigEntry tag val ': xs) tag val where
    getConfigEntry _ (ConsConfig x _) = unConfigEntry x
