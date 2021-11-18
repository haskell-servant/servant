{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines the error messages used in type-level errors.
-- Type-level errors can signal non-existing instances, for instance when
-- a combinator is not applied to the correct number of arguments.

module Servant.API.TypeErrors (
  PartialApplication,
  NoInstanceFor,
  NoInstanceForSub,
) where

import Data.Kind
import GHC.TypeLits

-- | No instance exists for @tycls (expr :> ...)@ because 
-- @expr@ is not recognised.
type NoInstanceForSub (tycls :: k) (expr :: k') =
  Text "There is no instance for " :<>: ShowType tycls
  :<>: Text " (" :<>: ShowType expr :<>: Text " :> ...)"

-- | No instance exists for @expr@.
type NoInstanceFor (expr :: k) =
  Text "There is no instance for " :<>: ShowType expr

-- | No instance exists for @tycls (expr :> ...)@ because @expr@ is not fully saturated.
type PartialApplication (tycls :: k) (expr :: k') =
  NoInstanceForSub tycls expr
  :$$: ShowType expr :<>: Text " expects " :<>: ShowType (Arity expr) :<>: Text " more arguments"

-- The arity of a combinator, i.e. the number of required arguments.
type Arity (ty :: k) = Arity' k

type family Arity' (ty :: k) :: Nat where
  Arity' (_ -> ty) = 1 + Arity' ty
  Arity' _ = 0
