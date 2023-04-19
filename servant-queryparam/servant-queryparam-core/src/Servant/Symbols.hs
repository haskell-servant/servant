{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- This module provides functions to modify 'Symbol's and type-level lists.
-- 
-- The workflow to define a modification function can be as follows.
-- 
-- 1. Use 'ToList' to convert a 'Symbol' to a type-level list of 'Symbol's. All 'Symbol's must consist of the @ASCII@ characters.
-- 
-- 1. Use functions from here and from the package [first-class-families](https://hackage.haskell.org/package/first-class-families) for type-level lists (@Fcf.Data.List@).
-- 
-- 1. Use 'FromList' to convert a type-level list of 'Symbol's back to a 'Symbol'.
-- 
-- Example (see 'DropPrefix'):
-- 
-- >>> :kind! DropPrefix "_userParams_category"
-- DropPrefix "_userParams_category" :: Symbol
-- = "category"
-- 
-- Modules that use the 'Eval' type family (e.g., "Servant.Record") must be imported together with modules that export instances of 'Eval'.
--
-- The @GHC@ documentation states that @Family instances are implicitly exported, just like class instances@.
module Servant.Symbols (
  Eval,
  Exp,
  FromList,
  FromList1,
  NotTyEq,
  DropPrefix,
  DropUnderscore,
  DropNotUnderscore,
) where

import Data.Symbol.Ascii (ToList)
import Fcf (Eval, Exp)
import qualified Fcf.Data.List as F
import GHC.Base (Symbol)
import GHC.TypeLits (AppendSymbol)
import qualified Fcf.Class.Ord as F

-- | Type inequality. See 'F.TyEq'.
data NotTyEq :: a -> b -> Exp Bool
type instance Eval (NotTyEq a b) = NotTyEqImpl a b

type family NotTyEqImpl (a :: k) (b :: l) :: Bool where
  NotTyEqImpl a a = 'False
  NotTyEqImpl a b = 'True

-- | Drop underscore 'Symbol's.
-- 
-- >>> :kind! Eval (DropUnderscore '["_", "_", "a"])
-- Eval (DropUnderscore '["_", "_", "a"]) :: [Symbol]
-- = '["a"]
type DropUnderscore = F.DropWhile (F.TyEq "_")

-- | Drop non-underscore 'Symbol's.
-- 
-- >>> :kind! Eval (DropNotUnderscore '["a", "_", "a"])
-- Eval (DropNotUnderscore '["a", "_", "a"]) :: [Symbol]
-- = '["_", "a"]
type DropNotUnderscore = F.DropWhile (NotTyEq "_")

-- | Drop the prefix of a 'Symbol'.
-- 
-- >>> :kind! DropPrefix "_userParams_category"
-- DropPrefix "_userParams_category" :: Symbol
-- = "category"
type family DropPrefix (sym :: Symbol) :: Symbol where 
  DropPrefix sym = FromList (Eval (DropUnderscore (Eval (DropNotUnderscore (Eval (DropUnderscore (ToList sym)))))))

-- | Convert a list of 'Symbol's to a 'Symbol'.
--
-- Works for ASCII-only 'Symbol's.
-- 
-- >>> :kind! FromList ["a", "b", "c"]
-- FromList ["a", "b", "c"] :: Symbol
-- = "abc"
-- 
-- >>> :kind! FromList ["a", "±", "c"]
-- FromList ["a", "±", "c"] :: Symbol
-- = AppendSymbol "a" (FromList1 (TypeError ...))
type family FromList (syms :: [Symbol]) :: Symbol where
  FromList xs = FromList1 (ToList (FromList1 xs))

-- | Convert a list of 'Symbol's to a 'Symbol'.
-- 
-- >>> :kind! FromList1 ["a", "b", "c"]
-- FromList1 ["a", "b", "c"] :: Symbol
-- = "abc"
-- 
-- Works for any 'Symbol's.
type family FromList1 (syms :: [Symbol]) :: Symbol where
  FromList1 '[] = ""
  FromList1 (x : xs) = AppendSymbol x (FromList1 xs)
