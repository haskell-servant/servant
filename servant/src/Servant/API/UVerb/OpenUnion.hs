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
, eject
)
where

import Data.SOP.Constraint
import Data.SOP.NS
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))
import GHC.TypeLits


-- * Stuff stolen from 'Data.WorldPeace" but for generics-sop
-- TODO: could much of this go into sop-core?

type IsMember (a :: u) (as :: [u]) = (CheckElemIsMember a as, UElem a as)

type family Contains (as :: [k]) (bs :: [k]) :: Constraint where
  Contains '[] _ = ()
  Contains (a ': as) bs = (IsMember a bs, Contains as bs)

type family Elem (x :: k) (xs :: [k]) :: Bool where
  Elem _ '[] = 'False
  Elem x (x' ': xs) =
    If (x == x') 'True (Elem x xs)

class UElem x xs where
  inject :: f x -> NS f xs
  eject :: NS f xs -> Maybe (f x)

instance {-# OVERLAPPING #-} UElem x (x ': xs) where
  inject = Z
  eject (Z x) = Just x
  eject _ = Nothing

instance {-# OVERLAPPING #-} UElem x xs => UElem x (x' ': xs) where
  inject = S . inject
  eject (Z _) = Nothing
  eject (S ns) = eject ns

type family Unique xs :: Constraint where
  Unique xs = If (Nubbed xs == 'True) (() :: Constraint) (TypeError (DuplicateElementError xs))

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
