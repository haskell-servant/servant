{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE EmptyCase #-}

-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Servant.API.MultiVerb
  ( -- * MultiVerb types
    MultiVerb,
    MultiVerb1,
    Respond,
    RespondAs,
    RespondEmpty,
    RespondStreaming,
    WithHeaders,
    DescHeader,
    OptHeader,
    AsHeaders (..),
    AsUnion (..),
    eitherToUnion,
    eitherFromUnion,
    maybeToUnion,
    maybeFromUnion,
    AsConstructor (..),
    GenericAsConstructor (..),
    GenericAsUnion (..),
    ResponseType,
    ResponseTypes,
  )
where

import Control.Applicative (Alternative(..), empty)
import qualified Data.CaseInsensitive as CI
import Data.Kind
import Data.Proxy
import Data.SOP
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Generics.SOP as GSOP
import GHC.TypeLits
import Network.HTTP.Types as HTTP
import Data.ByteString (ByteString)
import Control.Monad (ap, MonadPlus(..))

import Servant.API.TypeLevel.List
import Servant.API.Stream (SourceIO)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData, parseHeader, toHeader)
import Servant.API.UVerb.Union (Union)
import Servant.API.Header (Header')

-- | A type to describe a 'MultiVerb' response.
--
-- Includes status code, description, and return type. The content type of the
-- response is determined dynamically using the accept header and the list of
-- supported content types specified in the containing 'MultiVerb' type.
data Respond (s :: Nat) (desc :: Symbol) (a :: Type)

-- | A type to describe a 'MultiVerb' response with a fixed content type.
--
-- Similar to 'Respond', but hardcodes the content type to be used for
-- generating the response.
data RespondAs ct (s :: Nat) (desc :: Symbol) (a :: Type)

-- | A type to describe a 'MultiVerb' response with an empty body.
--
-- Includes status code and description.
type RespondEmpty s desc = RespondAs '() s desc ()

-- | A type to describe a streaming 'MultiVerb' response.
--
-- Includes status code, description, framing strategy and content type. Note
-- that the handler return type is hardcoded to be 'SourceIO ByteString'.
data RespondStreaming (s :: Nat) (desc :: Symbol) (framing :: Type) (ct :: Type)

-- | The result of parsing a response as a union alternative of type 'a'.
--
-- 'StatusMismatch' indicates that the response does not refer to the given
-- alternative, because the status code does not match the one produced by that
-- alternative.
--
-- 'UnrenderError' and 'UnrenderSuccess' represent respectively a failing and
-- successful parse of the response body as a value of type 'a'.
--
-- The 'UnrenderResult' type constructor has monad and alternative instances
-- corresponding to those of 'Either (Maybe (Last String)) a'.
data UnrenderResult a = StatusMismatch | UnrenderError String | UnrenderSuccess a
  deriving (Eq, Show, Functor)

instance Applicative UnrenderResult where
  pure = UnrenderSuccess
  (<*>) = ap

instance Monad UnrenderResult where
  return = pure
  StatusMismatch >>= _ = StatusMismatch
  UnrenderError e >>= _ = UnrenderError e
  UnrenderSuccess x >>= f = f x

instance Alternative UnrenderResult where
  empty = mzero
  (<|>) = mplus

instance MonadPlus UnrenderResult where
  mzero = StatusMismatch
  mplus StatusMismatch m = m
  mplus (UnrenderError e) StatusMismatch = UnrenderError e
  mplus (UnrenderError _) m = m
  mplus m@(UnrenderSuccess _) _ = m

type family ResponseType a :: Type

type instance ResponseType (Respond s desc a) = a

type instance ResponseType (RespondAs ct s desc a) = a

type instance ResponseType (RespondStreaming s desc framing ct) = SourceIO ByteString

-- | This type adds response headers to a 'MultiVerb' response.
--
-- Type variables:
--  * @hs@: type-level list of headers
--  * @a@: return type (with headers)
--  * @r@: underlying response (without headers)
data WithHeaders (hs :: [Type]) (a :: Type) (r :: Type)

-- | This is used to convert a response containing headers to a custom type
-- including the information in the headers.
class AsHeaders xs a b where
  fromHeaders :: (NP I xs, a) -> b
  toHeaders :: b -> (NP I xs, a)

-- single-header empty response
instance AsHeaders '[a] () a where
  toHeaders a = (I a :* Nil, ())
  fromHeaders = unI . hd . fst

-- single-header non-empty response, return value is a tuple of the response and the header
instance AsHeaders '[h] a (a, h) where
  toHeaders (t, cc) = (I cc :* Nil, t)
  fromHeaders (I cc :* Nil, t) = (t, cc)

data DescHeader (name :: Symbol) (desc :: Symbol) (a :: Type)

-- | A wrapper to turn a response header into an optional one.
data OptHeader h

class ServantHeaders hs xs | hs -> xs where
  constructHeaders :: NP I xs -> [HTTP.Header]
  extractHeaders :: Seq HTTP.Header -> Maybe (NP I xs)

instance ServantHeaders '[] '[] where
  constructHeaders Nil = []
  extractHeaders _ = Just Nil

headerName :: forall name. (KnownSymbol name) => HTTP.HeaderName
headerName =
  CI.mk
    . Text.encodeUtf8
    . Text.pack
    $ symbolVal (Proxy @name)

instance
  ( KnownSymbol name,
    ServantHeader h name x,
    FromHttpApiData x,
    ServantHeaders hs xs
  ) =>
  ServantHeaders (h ': hs) (x ': xs)
  where
  constructHeaders (I x :* xs) =
    constructHeader @h x
      <> constructHeaders @hs xs

  -- FUTUREWORK: should we concatenate all the matching headers instead of just
  -- taking the first one?
  extractHeaders hs = do
    let name' = headerName @name
        (hs0, hs1) = Seq.partition (\(h, _) -> h == name') hs
    x <- case hs0 of
      Seq.Empty -> empty
      ((_, h) :<| _) -> either (const empty) pure (parseHeader h)
    xs <- extractHeaders @hs hs1
    pure (I x :* xs)

class ServantHeader h (name :: Symbol) x | h -> name x where
  constructHeader :: x -> [HTTP.Header]

instance
  (KnownSymbol name, ToHttpApiData x) =>
  ServantHeader (Header' mods name x) name x
  where
  constructHeader x = [(headerName @name, toHeader x)]

instance
  (KnownSymbol name, ToHttpApiData x) =>
  ServantHeader (DescHeader name desc x) name x
  where
  constructHeader x = [(headerName @name, toHeader x)]

instance (ServantHeader h name x) => ServantHeader (OptHeader h) name (Maybe x) where
  constructHeader = foldMap (constructHeader @h)

type instance ResponseType (WithHeaders hs a r) = a

type family ResponseTypes (as :: [Type]) where
  ResponseTypes '[] = '[]
  ResponseTypes (a ': as) = ResponseType a ': ResponseTypes as

-- | This type can be used in Servant to produce an endpoint which can return
-- multiple values with various content types and status codes. It is similar to
-- 'UVerb' and behaves similarly, but it has some important differences:
--
--  * Descriptions and statuses can be attached to individual responses without
--    using wrapper types and without affecting the handler return type.
--  * The return type of the handler can be decoupled from the types of the
--    individual responses. One can use a 'Union' type just like for 'UVerb',
--    but 'MultiVerb' also supports using an arbitrary type with an 'AsUnion'
--    instance.
--  * Headers can be attached to individual responses, also without affecting
--    the handler return type.
data MultiVerb (method :: StdMethod) cs (as :: [Type]) (r :: Type)

-- | A 'MultiVerb' endpoint with a single response.
type MultiVerb1 m cs a = MultiVerb m cs '[a] (ResponseType a)


-- | This class is used to convert a handler return type to a union type
-- including all possible responses of a 'MultiVerb' endpoint.
--
-- Any glue code necessary to convert application types to and from the
-- canonical 'Union' type corresponding to a 'MultiVerb' endpoint should be
-- packaged into an 'AsUnion' instance.
class AsUnion (as :: [Type]) (r :: Type) where
  toUnion :: r -> Union (ResponseTypes as)
  fromUnion :: Union (ResponseTypes as) -> r

-- | Unions can be used directly as handler return types using this trivial
-- instance.
instance (rs ~ ResponseTypes as) => AsUnion as (Union rs) where
  toUnion = id
  fromUnion = id

-- | A handler with a single response.
instance (ResponseType r ~ a) => AsUnion '[r] a where
  toUnion = Z . I
  fromUnion = unI . unZ

_foo :: Union '[Int]
_foo = toUnion @'[Respond 200 "test" Int] @Int 3

class InjectAfter as bs where
  injectAfter :: Union bs -> Union (as .++ bs)

instance InjectAfter '[] bs where
  injectAfter = id

instance (InjectAfter as bs) => InjectAfter (a ': as) bs where
  injectAfter = S . injectAfter @as @bs

class InjectBefore as bs where
  injectBefore :: Union as -> Union (as .++ bs)

instance InjectBefore '[] bs where
  injectBefore x = case x of {}

instance (InjectBefore as bs) => InjectBefore (a ': as) bs where
  injectBefore (Z x) = Z x
  injectBefore (S x) = S (injectBefore @as @bs x)

eitherToUnion ::
  forall as bs a b.
  (InjectAfter as bs, InjectBefore as bs) =>
  (a -> Union as) ->
  (b -> Union bs) ->
  (Either a b -> Union (as .++ bs))
eitherToUnion f _ (Left a) = injectBefore @as @bs (f a)
eitherToUnion _ g (Right b) = injectAfter @as @bs (g b)

class EitherFromUnion as bs where
  eitherFromUnion ::
    (Union as -> a) ->
    (Union bs -> b) ->
    (Union (as .++ bs) -> Either a b)

instance EitherFromUnion '[] bs where
  eitherFromUnion _ g = Right . g

instance (EitherFromUnion as bs) => EitherFromUnion (a ': as) bs where
  eitherFromUnion f _ (Z x) = Left (f (Z x))
  eitherFromUnion f g (S x) = eitherFromUnion @as @bs (f . S) g x

maybeToUnion ::
  forall as a.
  (InjectAfter as '[()], InjectBefore as '[()]) =>
  (a -> Union as) ->
  (Maybe a -> Union (as .++ '[()]))
maybeToUnion f (Just a) = injectBefore @as @'[()] (f a)
maybeToUnion _ Nothing = injectAfter @as @'[()] (Z (I ()))

maybeFromUnion ::
  forall as a.
  (EitherFromUnion as '[()]) =>
  (Union as -> a) ->
  (Union (as .++ '[()]) -> Maybe a)
maybeFromUnion f = leftToMaybe . eitherFromUnion @as @'[()] f (const (Z (I ())))
  where
    leftToMaybe = either Just (const Nothing)

-- | This class can be instantiated to get automatic derivation of 'AsUnion'
-- instances via 'GenericAsUnion'. The idea is that one has to make sure that for
-- each response @r@ in a 'MultiVerb' endpoint, there is an instance of
-- @AsConstructor xs r@ for some @xs@, and that the list @xss@ of all the
-- corresponding @xs@ is equal to 'GSOP.Code' of the handler type. Then one can
-- write:
-- @
--   type Responses = ...
--   data Result = ...
--     deriving stock (Generic)
--     deriving (AsUnion Responses) via (GenericAsUnion Responses Result)
--
--   instance GSOP.Generic Result
-- @
-- and get an 'AsUnion' instance for free.
--
-- There are a few predefined instances for constructors taking a single type
-- corresponding to a simple response, and for empty responses, but in more
-- general cases one either has to define an 'AsConstructor' instance by hand,
-- or derive it via 'GenericAsConstructor'.
class AsConstructor xs r where
  toConstructor :: ResponseType r -> NP I xs
  fromConstructor :: NP I xs -> ResponseType r

class AsConstructors xss rs where
  toSOP :: Union (ResponseTypes rs) -> SOP I xss
  fromSOP :: SOP I xss -> Union (ResponseTypes rs)

instance AsConstructors '[] '[] where
  toSOP x = case x of {}
  fromSOP x = case x of {}

instance AsConstructor '[a] (Respond code desc a) where
  toConstructor x = I x :* Nil
  fromConstructor = unI . hd

instance AsConstructor '[a] (RespondAs (ct :: Type) code desc a) where
  toConstructor x = I x :* Nil
  fromConstructor = unI . hd

instance AsConstructor '[] (RespondEmpty code desc) where
  toConstructor _ = Nil
  fromConstructor _ = ()

newtype GenericAsConstructor r = GenericAsConstructor r

type instance ResponseType (GenericAsConstructor r) = ResponseType r

instance
  (GSOP.Code (ResponseType r) ~ '[xs], GSOP.Generic (ResponseType r)) =>
  AsConstructor xs (GenericAsConstructor r)
  where
  toConstructor = unZ . unSOP . GSOP.from
  fromConstructor = GSOP.to . SOP . Z

instance
  (AsConstructor xs r, AsConstructors xss rs) =>
  AsConstructors (xs ': xss) (r ': rs)
  where
  toSOP (Z (I x)) = SOP . Z $ toConstructor @xs @r x
  toSOP (S x) = SOP . S . unSOP $ toSOP @xss @rs x

  fromSOP (SOP (Z x)) = Z (I (fromConstructor @xs @r x))
  fromSOP (SOP (S x)) = S (fromSOP @xss @rs (SOP x))

-- | This type is meant to be used with @deriving via@ in order to automatically
-- generate an 'AsUnion' instance using 'Generics.SOP'.
--
-- See 'AsConstructor' for more information and examples.
newtype GenericAsUnion rs a = GenericAsUnion a

instance
  (GSOP.Code a ~ xss, GSOP.Generic a, AsConstructors xss rs) =>
  AsUnion rs (GenericAsUnion rs a)
  where
  toUnion (GenericAsUnion x) = fromSOP @xss @rs (GSOP.from x)
  fromUnion = GenericAsUnion . GSOP.to . toSOP @xss @rs

-- | A handler for a pair of empty responses can be implemented simply by
-- returning a boolean value. The convention is that the "failure" case, normally
-- represented by 'False', corresponds to the /first/ response.
instance
  AsUnion
    '[ RespondEmpty s1 desc1,
       RespondEmpty s2 desc2
     ]
    Bool
  where
  toUnion False = Z (I ())
  toUnion True = S (Z (I ()))

  fromUnion (Z (I ())) = False
  fromUnion (S (Z (I ()))) = True
  fromUnion (S (S x)) = case x of {}

-- | A handler for a pair of responses where the first is empty can be
-- implemented simply by returning a 'Maybe' value. The convention is that the
-- "failure" case, normally represented by 'Nothing', corresponds to the /first/
-- response.
instance
  (ResponseType r1 ~ (), ResponseType r2 ~ a) =>
  AsUnion '[r1, r2] (Maybe a)
  where
  toUnion Nothing = Z (I ())
  toUnion (Just x) = S (Z (I x))

  fromUnion (Z (I ())) = Nothing
  fromUnion (S (Z (I x))) = Just x
  fromUnion (S (S x)) = case x of {}
