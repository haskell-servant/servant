{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type-level code for implementing and using 'UVerb'.
module Servant.API.UVerb.OpenUnion
( IsMember
, Unique
, inject
)
where

import Data.SOP.Constraint
import Data.SOP.NS
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))
import GHC.TypeLits


-- * Stuff stolen from 'Data.WorldPeace" but for generics-sop
-- TODO: could much of this go into sop-core?

type IsMember (a :: u) (as :: [u]) = (CheckElemIsMember a as, UElem a as (RIndex a as))

type family Contains (as :: [k]) (bs :: [k]) :: Constraint where
  Contains '[] _ = ()
  Contains (a ': as) bs = (IsMember a bs, Contains as bs)

data Nat_ = S_ Nat_ | Z_


-- | TODO: we probably can make do without this.
type family RIndex (r :: k) (rs :: [k]) :: Nat_ where
  RIndex r (r ': rs) = 'Z_
  RIndex r (s ': rs) = 'S_ (RIndex r rs)

type family Elem (x :: k) (xs :: [k]) :: Bool where
    Elem _ '[]       = 'False
    Elem x (x ': xs) = 'True
    Elem x (y ': xs) = Elem x xs


class i ~ RIndex a as => UElem (a :: k) (as :: [k]) (i :: Nat_) where
  inject :: f a -> NS f as
  match  :: NS f as -> Maybe (f a)

instance UElem a (a ': as) 'Z_ where
  inject x = Z x
  match y = case y of
    Z x -> Just x
    _ -> Nothing

instance ( RIndex a (b ': as) ~ ('S_ i) , UElem a as i) => UElem a (b ': as) ('S_ i) where
  inject x = S (inject x)
  match y = case y of
    Z _ -> Nothing
    S z -> match z

type family Unique xs :: Constraint where
  Unique xs = If (Nubbed xs == 'True) (() ~ ()) (TypeError (DuplicateElementError xs))

type family Nubbed xs :: Bool where
  Nubbed '[] = 'True
  Nubbed (x ': xs) = If (Elem x xs) 'False (Nubbed xs)

_testNubbed :: ( ( Nubbed '[Bool, Int, Int] ~ 'False
                 , Nubbed '[Int, Int, Bool] ~ 'False
                 , Nubbed '[Int, Bool] ~ 'True
                 )
               => a) -> a
_testNubbed = id

-- | Check whether @a@ is in list.  This will throw nice errors if the element is not in the
-- list, or if there is a duplicate in the list.
type family CheckElemIsMember (a :: k) (as :: [k]) :: Constraint where
    CheckElemIsMember a as =
      If (Elem a as)
        (If (Nubbed as)
          (() :: Constraint)
          (TypeError (DuplicateElementError as)))
        (TypeError (NoElementError a as))

type NoElementError (r :: k) (rs :: [k]) =
          'Text "Expected one of:"
    ':$$: 'Text "    " ':<>: 'ShowType rs
    ':$$: 'Text "But got:"
    ':$$: 'Text "    " ':<>: 'ShowType r

type DuplicateElementError (rs :: [k]) =
          'Text "Duplicate element in list:"
    ':$$: 'Text "    " ':<>: 'ShowType rs
