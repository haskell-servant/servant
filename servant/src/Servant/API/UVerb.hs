{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
    HasStatus (StatusOf),
    statusOf,
    HasStatuses (Statuses, statuses),
    WithStatus (..),
    module Servant.API.UVerb.Union,
  )
where

import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (Nat)
import Network.HTTP.Types (Status, StdMethod)
import Servant.API.ContentTypes (JSON, PlainText, FormUrlEncoded, OctetStream, NoContent, MimeRender(mimeRender), MimeUnrender(mimeUnrender))
import Servant.API.Status (KnownStatus, statusVal)
import Servant.API.UVerb.Union

class KnownStatus (StatusOf a) => HasStatus (a :: *) where
  type StatusOf (a :: *) :: Nat

statusOf :: forall a proxy. HasStatus a => proxy a -> Status
statusOf = const (statusVal (Proxy :: Proxy (StatusOf a)))

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

-- | A simple newtype wrapper that pairs a type with its status code.  It
-- implements all the content types that Servant ships with by default.
newtype WithStatus (k :: Nat) a = WithStatus a
  deriving (Eq, Show)

-- | an instance of this typeclass assigns a HTTP status code to a return type
--
-- Example:
--
-- @
--    data NotFoundError = NotFoundError String
--
--    instance HasStatus NotFoundError where
--      type StatusOf NotFoundError = 404
-- @
--
-- You can also use the convience newtype wrapper 'WithStatus' if you want to
-- avoid writing a 'HasStatus' instance manually. It also has the benefit of
-- showing the status code in the type; which might aid in readability.
instance KnownStatus n => HasStatus (WithStatus n a) where
  type StatusOf (WithStatus n a) = n


-- | A variant of 'Verb' that can have any of a number of response values and status codes.
--
-- FUTUREWORK: it would be nice to make 'Verb' a special case of 'UVerb', and only write
-- instances for 'HasServer' etc. for the latter, getting them for the former for free.
-- Something like:
--
-- @type Verb method statusCode contentTypes a = UVerb method contentTypes [WithStatus statusCode a]@
--
-- Backwards compatibility is tricky, though: this type alias would mean people would have to
-- use 'respond' instead of 'pure' or 'return', so all old handlers would have to be rewritten.
data UVerb (method :: StdMethod) (contentTypes :: [*]) (as :: [*])

instance {-# OVERLAPPING #-} MimeRender JSON a => MimeRender JSON (WithStatus _status a) where
  mimeRender contentTypeProxy (WithStatus a) = mimeRender contentTypeProxy a

instance {-# OVERLAPPING #-} MimeRender PlainText a => MimeRender PlainText (WithStatus _status a) where
  mimeRender contentTypeProxy (WithStatus a) = mimeRender contentTypeProxy a

instance {-# OVERLAPPING #-} MimeRender FormUrlEncoded a => MimeRender FormUrlEncoded (WithStatus _status a) where
  mimeRender contentTypeProxy (WithStatus a) = mimeRender contentTypeProxy a

instance {-# OVERLAPPING #-} MimeRender OctetStream a => MimeRender OctetStream (WithStatus _status a) where
  mimeRender contentTypeProxy (WithStatus a) = mimeRender contentTypeProxy a

instance {-# OVERLAPPING #-} MimeUnrender JSON a => MimeUnrender JSON (WithStatus _status a) where
  mimeUnrender contentTypeProxy input = WithStatus <$> mimeUnrender contentTypeProxy input

instance {-# OVERLAPPING #-} MimeUnrender PlainText a => MimeUnrender PlainText (WithStatus _status a) where
  mimeUnrender contentTypeProxy input = WithStatus <$> mimeUnrender contentTypeProxy input

instance {-# OVERLAPPING #-} MimeUnrender FormUrlEncoded a => MimeUnrender FormUrlEncoded (WithStatus _status a) where
  mimeUnrender contentTypeProxy input = WithStatus <$> mimeUnrender contentTypeProxy input

instance {-# OVERLAPPING #-} MimeUnrender OctetStream a => MimeUnrender OctetStream (WithStatus _status a) where
  mimeUnrender contentTypeProxy input = WithStatus <$> mimeUnrender contentTypeProxy input
