{-# LANGUAGE CPP                     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif
module Servant.Swagger.Internal.TypeLevel.Every where

import           Data.Proxy
import           GHC.Exts                                (Constraint)

import           Servant.Swagger.Internal.TypeLevel.TMap

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XGADTs
-- >>> :set -XRankNTypes
-- >>> :set -XScopedTypeVariables
-- >>> import GHC.TypeLits
-- >>> import Data.List

-- | Apply multiple constraint constructors to a type.
--
-- @
-- EveryTF '[Show, Read] a ~ (Show a, Read a)
-- @
--
-- Note that since this is a type family, you have to alway fully apply @'EveryTF'@.
--
-- For partial application of multiple constraint constructors see @'Every'@.
type family EveryTF cs x :: Constraint where
  EveryTF '[] x = ()
  EveryTF (c ': cs) x = (c x, EveryTF cs x)

-- | Apply multiple constraint constructors to a type as a class.
--
-- This is different from @'EveryTF'@ in that it allows partial application.
class EveryTF cs x => Every (cs :: [* -> Constraint]) (x :: *) where

instance Every '[] x where
instance (c x, Every cs x) => Every (c ': cs) x where

-- | Like @'tmap'@, but uses @'Every'@ for multiple constraints.
--
-- >>> let zero :: forall p a. (Show a, Num a) => p a -> String; zero _ = show (0 :: a)
-- >>> tmapEvery (Proxy :: Proxy [Show, Num]) zero (Proxy :: Proxy [Int, Float]) :: [String]
-- ["0","0.0"]
tmapEvery :: forall a cs p p'' xs. (TMap (Every cs) xs) =>
  p cs -> (forall x p'. Every cs x => p' x -> a) -> p'' xs -> [a]
tmapEvery _ = tmap (Proxy :: Proxy (Every cs))
