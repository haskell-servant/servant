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

{-

Copyright Dennis Gosnell (c) 2017-2018

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Author name here nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

-- | Type-level code for implementing and using 'UVerb'.  Heavily inspired by
-- [world-peace](https://github.com/cdepillabout/world-peace).
module Servant.API.UVerb.Union
( IsMember
, Unique
, Union
, inject
, eject
, foldMapUnion
, matchUnion
)
where

import Data.Proxy (Proxy)
import Data.SOP.BasicFunctors (I, unI)
import Data.SOP.Constraint
import Data.SOP.NS
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))
import GHC.TypeLits

type Union = NS I

-- | Convenience function to apply a function to an unknown union element using a type class.
-- All elements of the union must have instances in the type class, and the function is
-- applied unconditionally.
--
-- See also: 'matchUnion'.
foldMapUnion ::
  forall (c :: * -> Constraint) (a :: *) (as :: [*]).
  All c as =>
  Proxy c ->
  (forall x. c x => x -> a) ->
  Union as ->
  a
foldMapUnion proxy go = cfoldMap_NS proxy (go . unI)

-- | Convenience function to extract a union element using 'cast', ie. return the value if the
-- selected type happens to be the actual type of the union in this value, or 'Nothing'
-- otherwise.
--
-- See also: 'foldMapUnion'.
matchUnion :: forall (a :: *) (as :: [*]). (IsMember a as) => Union as -> Maybe a
matchUnion = fmap unI . eject

-- * Stuff stolen from 'Data.WorldPeace" but for generics-sop

-- (this could to go sop-core, except it's probably too specialized to the servant use-case.)

type IsMember (a :: u) (as :: [u]) = (Unique as, CheckElemIsMember a as, UElem a as)

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

-- | Check whether @a@ is in list.  This will throw nice errors if the element is not in the
-- list, or if there is a duplicate in the list.
type family CheckElemIsMember (a :: k) (as :: [k]) :: Constraint where
    CheckElemIsMember a as =
      If (Elem a as) (() :: Constraint) (TypeError (NoElementError a as))

type NoElementError (r :: k) (rs :: [k]) =
          'Text "Expected one of:"
    ':$$: 'Text "    " ':<>: 'ShowType rs
    ':$$: 'Text "But got:"
    ':$$: 'Text "    " ':<>: 'ShowType r

type DuplicateElementError (rs :: [k]) =
          'Text "Duplicate element in list:"
    ':$$: 'Text "    " ':<>: 'ShowType rs

type family Elem (x :: k) (xs :: [k]) :: Bool where
  Elem _ '[] = 'False
  Elem x (x' ': xs) =
    If (x == x') 'True (Elem x xs)

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
_testNubbed a = a
