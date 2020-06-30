{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | An alternative to 'Verb' for end-points that respond with a resource value of any of an
-- open union of types, and specific status codes for each type in this union.  (`UVerb` is
-- short for `UnionVerb`)
--
-- This can be used for returning (rather than throwing) exceptions in a server as in, say
-- @'[Report, WaiError]@; or responding with either a 303 forward with a location header, or
-- 201 created with a different body type, depending on the circumstances.  (All of this can
-- be done with vanilla servant-server by throwing exceptions, but it can't be represented in
-- the API types without something like `UVerb`.)
--
-- See <https://docs.servant.dev/en/stable/cookbook/uverb/UVerb.html> for a working example.
module Servant.API.UVerb
  ( UVerb,
    Union,
    HasStatus,
    StatusOf,
    statusOf,
    Statuses,
    WithStatus (..),
    module Servant.API.UVerb.OpenUnion,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Identity (Identity)
import Data.SOP.NS (NS)
import Data.Typeable (Proxy (Proxy))
import qualified GHC.Generics as GHC
import GHC.TypeLits (Nat)
import Network.HTTP.Types (Status, StdMethod)
import Servant.API.ContentTypes (NoContent)
import Servant.API.Status (KnownStatus, statusVal)
import Servant.API.UVerb.OpenUnion

class KnownStatus (StatusOf a) => HasStatus (a :: *) where
  type StatusOf (a :: *) :: Nat

statusOf :: forall a proxy. HasStatus a => proxy a -> Status
statusOf = const (statusVal (Proxy :: Proxy (StatusOf a)))

type family Statuses (as :: [*]) :: [Nat]

type instance Statuses '[] = '[]

type instance Statuses (a ': as) = StatusOf a ': Statuses as

newtype WithStatus (k :: Nat) a = WithStatus a
  deriving (Eq, Show, GHC.Generic)

instance (GHC.Generic (WithStatus n a), ToJSON a) => ToJSON (WithStatus n a)

instance (GHC.Generic (WithStatus n a), FromJSON a) => FromJSON (WithStatus n a)

instance KnownStatus n => HasStatus (WithStatus n a) where
  type StatusOf (WithStatus n a) = n

-- | If an API can respond with 'NoContent' we assume that this will happen
-- with the status code 204 No Content. If this needs to be overridden,
-- 'WithStatus' can be used.
instance HasStatus NoContent where
  type StatusOf NoContent = 204

-- FUTUREWORK:
-- @type Verb method statusCode contentTypes a = UVerb method contentTypes [WithStatus statusCode a]@
-- no, wait, this is not the same.  this would mean people would have to use 'respond' instead
-- of 'pure' or 'return'.
data UVerb (method :: StdMethod) (contentTypes :: [*]) (as :: [*])

type Union = NS Identity
