{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.Swagger.Internal.TypeLevel.API where

import           GHC.Exts       (Constraint)
import           Servant.API

-- | Build a list of endpoints from an API.
type family EndpointsList api where
  EndpointsList (a :<|> b) = AppendList (EndpointsList a) (EndpointsList b)
  EndpointsList (e :> a)   = MapSub e (EndpointsList a)
  EndpointsList a = '[a]

-- | Check whether @sub@ is a sub API of @api@.
type family IsSubAPI sub api :: Constraint where
  IsSubAPI sub api = AllIsElem (EndpointsList sub) api

-- | Check that every element of @xs@ is an endpoint of @api@.
type family AllIsElem xs api :: Constraint where
  AllIsElem '[] api = ()
  AllIsElem (x ': xs) api = (IsIn x api, AllIsElem xs api)

-- | Apply @(e :>)@ to every API in @xs@.
type family MapSub e xs where
  MapSub e '[] = '[]
  MapSub e (x ': xs) = (e :> x) ': MapSub e xs

-- | Append two type-level lists.
type family AppendList xs ys where
  AppendList '[]       ys = ys
  AppendList (x ': xs) ys = x ': AppendList xs ys

type family Or (a :: Constraint) (b :: Constraint) :: Constraint where
  Or () b = ()
  Or a () = ()

type family IsIn sub api :: Constraint where
  IsIn e (a :<|> b) = Or (IsIn e a) (IsIn e b)
  IsIn (e :> a) (e :> b) = IsIn a b
  IsIn e e = ()

-- | Check whether a type is a member of a list of types.
-- This is a type-level analogue of @'elem'@.
type family Elem x xs where
  Elem x '[] = 'False
  Elem x (x ': xs) = 'True
  Elem x (y ': xs) = Elem x xs

-- | Remove duplicates from a type-level list.
type family Nub xs where
  Nub '[] = '[]
  Nub (x ': xs) = x ': Nub (Remove x xs)

-- | Remove element from a type-level list.
type family Remove x xs where
  Remove x '[]       = '[]
  Remove x (x ': ys) =      Remove x ys
  Remove x (y ': ys) = y ': Remove x ys

-- | Extract a list of unique "body" types for a specific content-type from a servant API.
type BodyTypes c api = Nub (BodyTypes' c api)

-- | @'AddBodyType' c cs a as@ adds type @a@ to the list @as@
-- only if @c@ is in @cs@.
type AddBodyType c cs a as = If (Elem c cs) (a ': as) as

-- | Extract a list of "body" types for a specific content-type from a servant API.
-- To extract unique types see @'BodyTypes'@.
--
-- @'NoContent'@ is removed from the list and not tested.  (This allows for leaving the body
-- completely empty on responses to requests that only accept 'application/json', while
-- setting the content-type in the response accordingly.)
type family BodyTypes' c api :: [*] where
  BodyTypes' c (Verb verb b cs (Headers hdrs a)) = AddBodyType c cs a '[]
  BodyTypes' c (Verb verb b cs NoContent) = '[]
  BodyTypes' c (Verb verb b cs a) = AddBodyType c cs a '[]
  BodyTypes' c (ReqBody' mods cs a :> api) = AddBodyType c cs a (BodyTypes' c api)
  BodyTypes' c (e :> api) = BodyTypes' c api
  BodyTypes' c (a :<|> b) = AppendList (BodyTypes' c a) (BodyTypes' c b)
  BodyTypes' c api = '[]

