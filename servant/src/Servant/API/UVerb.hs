{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
    HasStatus (StatusOf),
    statusOf,
    HasStatuses (Statuses, statuses),
    WithStatus (..),
    module Servant.API.UVerb.OpenUnion,
    collapseUResp,
    extractUResp,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.SOP.BasicFunctors (I, K (K), unI)
import Data.SOP.Constraint (All, Constraint)
import Data.SOP.NS (NS, cmap_NS, collapse_NS)
import Data.Proxy (Proxy (Proxy))
import qualified GHC.Generics as GHC
import GHC.TypeLits (Nat)
import Network.HTTP.Types (Status, StdMethod)
import Servant.API.ContentTypes (MimeRender (mimeRender), MimeUnrender (mimeUnrender), NoContent)
import Servant.API.Status (KnownStatus, statusVal)
import Servant.API.UVerb.OpenUnion

class KnownStatus (StatusOf a) => HasStatus (a :: *) where
  type StatusOf (a :: *) :: Nat

statusOf :: forall a proxy. HasStatus a => proxy a -> Status
statusOf = const (statusVal (Proxy :: Proxy (StatusOf a)))

instance KnownStatus n => HasStatus (WithStatus n a) where
  type StatusOf (WithStatus n a) = n

-- | If an API can respond with 'NoContent' we assume that this will happen
-- with the status code 204 No Content. If this needs to be overridden,
-- 'WithStatus' can be used.
instance HasStatus NoContent where
  type StatusOf NoContent = 204

class HasStatuses (as :: [*]) where
  type Statuses (as :: [*]) :: [Nat]
  statuses :: Proxy as -> [Status]

instance HasStatuses '[] where
  type Statuses '[] = '[]
  statuses _ = []

instance (HasStatus a, HasStatuses as) => HasStatuses (a ': as) where
  type Statuses (a ': as) = StatusOf a ': Statuses as
  statuses _ = statusOf (Proxy :: Proxy a) : statuses (Proxy :: Proxy as)

newtype WithStatus (k :: Nat) a = WithStatus a
  deriving (Eq, Show, GHC.Generic)

instance (GHC.Generic (WithStatus n a), ToJSON a) => ToJSON (WithStatus n a)

instance (GHC.Generic (WithStatus n a), FromJSON a) => FromJSON (WithStatus n a)

instance MimeRender ctype a => MimeRender ctype (WithStatus _status a) where
  mimeRender contentTypeProxy (WithStatus a) = mimeRender contentTypeProxy a

instance MimeUnrender ctype a => MimeUnrender ctype (WithStatus _status a) where
  mimeUnrender contentTypeProxy input =
    WithStatus <$> mimeUnrender contentTypeProxy input

-- FUTUREWORK:
-- @type Verb method statusCode contentTypes a = UVerb method contentTypes [WithStatus statusCode a]@
-- no, wait, this is not the same.  this would mean people would have to use 'respond' instead
-- of 'pure' or 'return'.
data UVerb (method :: StdMethod) (contentTypes :: [*]) (as :: [*])

type Union = NS I

-- | Convenience function to apply a function to an unknown union element using a type class.
-- All elements of the union must have instances in the type class, and the function is
-- applied unconditionally.
--
-- See also: 'extractUResp'.
collapseUResp ::
  forall (c :: * -> Constraint) (a :: *) (as :: [*]).
  All c as =>
  Proxy c -> (forall x. c x => x -> a) -> Union as -> a
collapseUResp proxy render = collapse_NS . cmap_NS proxy (K . render . unI)

-- | Convenience function to extract a union element using 'cast', ie. return the value if the
-- selected type happens to be the actual type of the union in this value, or 'Nothing'
-- otherwise.
--
-- See also: 'collapseUResp'.
extractUResp :: forall (a :: *) (as :: [*]). (IsMember a as) => Union as -> Maybe a
extractUResp = fmap unI . match
