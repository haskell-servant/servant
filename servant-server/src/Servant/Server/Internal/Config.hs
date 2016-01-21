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

-- | When calling 'Servant.Server.serve' you have to supply a configuration
-- value of type @'Config' configTypes@. This parameter is used to pass values
-- to combinators. (It shouldn't be confused with general configuration
-- parameters for your web app, like the port, etc.). If you don't use
-- combinators that require any config entries, you can just pass 'EmptyConfig'.
-- To create a config with entries, use the operator @(':.')@. The parameter of
-- the type 'Config' is a type-level list reflecting the types of the contained
-- config entries:
--
-- >>> :type True :. () :. EmptyConfig
-- True :. () :. EmptyConfig :: Config '[Bool, ()]
data Config configTypes where
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

-- | This class is used to access config entries in 'Config's. 'getConfigEntry'
-- returns the first value where the type matches:
--
-- >>> getConfigEntry (True :. False :. EmptyConfig) :: Bool
-- True
--
-- If the 'Config' does not contain an entry of the requested type, you'll get
-- an error:
--
-- >>> getConfigEntry (True :. False :. EmptyConfig) :: String
-- ...
--     No instance for (HasConfigEntry '[] [Char])
-- ...
class HasConfigEntry (config :: [*]) (val :: *) where
    getConfigEntry :: Config config -> val

instance OVERLAPPABLE_
         HasConfigEntry xs val => HasConfigEntry (notIt ': xs) val where
    getConfigEntry (_ :. xs) = getConfigEntry xs

instance OVERLAPPING_
         HasConfigEntry (val ': xs) val where
    getConfigEntry (x :. _) = x

-- * support for named subconfigs

-- | Normally config entries are accessed by their types. In case you need
-- to have multiple values of the same type in your 'Config' and need to access
-- them, we provide 'NamedConfig'. You can think of it as sub-namespaces for
-- 'Config's.
data NamedConfig (name :: Symbol) (subConfig :: [*])
  = NamedConfig (Config subConfig)

-- | 'descendIntoNamedConfig' allows you to access `NamedConfig's. Usually you
-- won't have to use it yourself but instead use a combinator like
-- 'Servant.API.WithNamedConfig.WithNamedConfig'.
--
-- This is how 'descendIntoNamedConfig' works:
--
-- >>> :set -XFlexibleContexts
-- >>> let subConfig = True :. EmptyConfig
-- >>> :type subConfig
-- subConfig :: Config '[Bool]
-- >>> let parentConfig = False :. (NamedConfig subConfig :: NamedConfig "subConfig" '[Bool]) :. EmptyConfig
-- >>> :type parentConfig
-- parentConfig :: Config '[Bool, NamedConfig "subConfig" '[Bool]]
-- >>> descendIntoNamedConfig (Proxy :: Proxy "subConfig") parentConfig :: Config '[Bool]
-- True :. EmptyConfig
descendIntoNamedConfig :: forall config name subConfig .
  HasConfigEntry config (NamedConfig name subConfig) =>
  Proxy (name :: Symbol) -> Config config -> Config subConfig
descendIntoNamedConfig Proxy config =
  let NamedConfig subConfig = getConfigEntry config :: NamedConfig name subConfig
  in subConfig
