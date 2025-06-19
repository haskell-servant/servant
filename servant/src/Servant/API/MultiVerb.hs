{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE EmptyCase #-}

-- | MultiVerb is a part of the type-level eDSL that allows you to express complex routes
-- while retaining a high level of precision with good ergonomics.
module Servant.API.MultiVerb
  ( -- ** MultiVerb types
    MultiVerb
  , MultiVerb1

    -- ** Response types
  , Respond
  , RespondAs
  , RespondEmpty
  , RespondStreaming

    -- ** Headers
  , WithHeaders
  , DescHeader
  , OptHeader
  , AsHeaders (..)
  , ServantHeaders (..)
  , ServantHeader (..)

    -- ** Unions of responses
  , AsUnion (..)
  , eitherToUnion
  , eitherFromUnion
  , maybeToUnion
  , maybeFromUnion

    -- ** Internal machinery
  , AsConstructor (..)
  , GenericAsConstructor (..)
  , GenericAsUnion (..)
  , ResponseType
  , ResponseTypes
  , UnrenderResult (..)
  )
where

import Control.Applicative (Alternative (..), empty)
import Control.Monad (MonadPlus (..), ap)
import Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI
import Data.Kind
import Data.Proxy
import Data.SOP
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.TypeLits
import Generics.SOP as GSOP
import Network.HTTP.Types as HTTP
import Servant.API.Header (Header')
import Servant.API.Stream (SourceIO)
import Servant.API.TypeLevel.List
import Servant.API.UVerb.Union (Union)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData, parseHeader, toHeader)

-- | A type to describe a 'MultiVerb' response.
--
-- Includes status code, description, and return type. The content type of the
-- response is determined dynamically using the accept header and the list of
-- supported content types specified in the containing 'MultiVerb' type.
data Respond (s :: Nat) (description :: Symbol) (a :: Type)

-- | A type to describe a 'MultiVerb' response with a fixed content type.
--
-- Similar to 'Respond', but hardcodes the content type to be used for
-- generating the response. This content type is distinct from the one
-- given to 'MultiVerb', as it dictactes the response's content type, not the
-- content type request that is to be accepted.
data RespondAs responseContentType (s :: Nat) (description :: Symbol) (a :: Type)

-- | A type to describe a 'MultiVerb' response with an empty body.
--
-- Includes status code and description.
type RespondEmpty s description = RespondAs '() s description ()

-- | A type to describe a streaming 'MultiVerb' response.
--
-- Includes status code, description, framing strategy and content type. Note
-- that the handler return type is hardcoded to be 'SourceIO ByteString'.
data RespondStreaming (s :: Nat) (description :: Symbol) (framing :: Type) (ct :: Type)

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

type instance ResponseType (Respond s description a) = a

type instance ResponseType (RespondAs responseContentType s description a) = a

type instance ResponseType (RespondStreaming s description framing ct) = SourceIO ByteString

-- | This type adds response headers to a 'MultiVerb' response.
data WithHeaders (headers :: [Type]) (returnType :: Type) (response :: Type)

-- | This is used to convert a response containing headers to a custom type
-- including the information in the headers.
--
-- If you need to send a combination of headers and response that is not provided by Servant,
-- you can cwrite your own instance. Take example on the ones provided.
class AsHeaders headers response returnType where
  fromHeaders :: (NP I headers, response) -> returnType
  toHeaders :: returnType -> (NP I headers, response)

-- | Single-header empty response
instance AsHeaders '[a] () a where
  toHeaders a = (I a :* Nil, ())
  fromHeaders = unI . hd . fst

-- | Single-header non-empty response, return value is a tuple of the response and the header
instance AsHeaders '[h] a (a, h) where
  toHeaders (t, cc) = (I cc :* Nil, t)
  fromHeaders (I cc :* Nil, t) = (t, cc)

-- | Two headers and an empty response, return value is a tuple of the response and the header
instance AsHeaders '[a, b] () (a, b) where
  toHeaders (h1, h2) = (I h1 :* I h2 :* Nil, ())
  fromHeaders (I h1 :* I h2 :* Nil, ()) = (h1, h2)

data DescHeader (name :: Symbol) (description :: Symbol) (a :: Type)

-- | A wrapper to turn a response header into an optional one.
data OptHeader h

class ServantHeaders headers xs | headers -> xs where
  constructHeaders :: NP I xs -> [HTTP.Header]
  extractHeaders :: Seq HTTP.Header -> Maybe (NP I xs)

instance ServantHeaders '[] '[] where
  constructHeaders Nil = []
  extractHeaders _ = Just Nil

headerName :: forall name. KnownSymbol name => HTTP.HeaderName
headerName =
  CI.mk
    . Text.encodeUtf8
    . Text.pack
    $ symbolVal (Proxy @name)

instance
  ( KnownSymbol name
  , ServantHeader h name x
  , FromHttpApiData x
  , ServantHeaders headers xs
  )
  => ServantHeaders (h ': headers) (x ': xs)
  where
  constructHeaders (I x :* xs) =
    constructHeader @h x
      <> constructHeaders @headers xs

  -- NOTE: should we concatenate all the matching headers instead of just taking the first one?
  extractHeaders headers = do
    let name' = headerName @name
        (headers0, headers1) = Seq.partition (\(h, _) -> h == name') headers
    x <- case headers0 of
      Seq.Empty -> empty
      ((_, h) :<| _) -> either (const empty) pure (parseHeader h)
    xs <- extractHeaders @headers headers1
    pure (I x :* xs)

class ServantHeader h (name :: Symbol) x | h -> name x where
  constructHeader :: x -> [HTTP.Header]

instance
  (KnownSymbol name, ToHttpApiData x)
  => ServantHeader (Header' mods name x) name x
  where
  constructHeader x = [(headerName @name, toHeader x)]

instance
  (KnownSymbol name, ToHttpApiData x)
  => ServantHeader (DescHeader name description x) name x
  where
  constructHeader x = [(headerName @name, toHeader x)]

instance ServantHeader h name x => ServantHeader (OptHeader h) name (Maybe x) where
  constructHeader = foldMap (constructHeader @h)

type instance ResponseType (WithHeaders headers returnType response) = returnType

type family ResponseTypes (as :: [Type]) where
  ResponseTypes '[] = '[]
  ResponseTypes (a ': as) = ResponseType a ': ResponseTypes as

-- | 'MultiVerb' produces an endpoint which can return
-- multiple values with various content types and status codes. It is similar to
-- 'Servant.API.UVerb.UVerb' and behaves similarly, but it has some important differences:
--
--  * Descriptions and statuses can be attached to individual responses without
--    using wrapper types and without affecting the handler return type.
--  * The return type of the handler can be decoupled from the types of the
--    individual responses. One can use a 'Union' type just like for 'Servant.API.UVerb.UVerb',
--    but 'MultiVerb' also supports using an arbitrary type with an 'AsUnion'
--    instance. Each response is responsible for their content type.
--  * Headers can be attached to individual responses, also without affecting
--    the handler return type.
--
-- ==== __Example__
-- Let us create an endpoint that captures an 'Int' and has the following logic:
--
-- * If the number is negative, we return status code 400 and an empty body;
-- * If the number is even, we return a 'Bool' in the response body;
-- * If the number is odd, we return another 'Int' in the response body.
--
-- >  import qualified Generics.SOP as GSOP
--
-- > -- All possible HTTP responses
-- > type Responses =
-- >   '[ type RespondEmpty 400 "Negative"
-- >    , type Respond 200 "Even number" Bool
-- >    , type Respond 200 "Odd number" Int
-- >    ]
-- >
-- > -- All possible return types
-- > data Result
-- >   = NegativeNumber
-- >   | Odd Int
-- >   | Even Bool
-- >   deriving stock (Generic)
-- >   deriving (AsUnion Responses)
-- >     via GenericAsUnion Responses Result
-- >
-- > instance GSOP.Generic Result
--
-- These deriving statements above tie together the responses and the return values, and the order in which they are defined matters. For instance, if @Even@ and @Odd@ had switched places in the definition of @Result@, this would provoke an error:
--
--
-- > • No instance for ‘AsConstructor
-- >     ((:) @Type Int ('[] @Type)) (Respond 200 "Even number" Bool)’
-- >         arising from the 'deriving' clause of a data type declaration
--
-- If you would prefer to write an intance of 'AsUnion' by yourself, read more in the typeclass' documentation.
--
-- Finally, let us write our endpoint description:
--
-- > type MultipleChoicesInt =
-- >   Capture "int" Int
-- >   :> MultiVerb
-- >     'GET
-- >     '[JSON]
-- >     Responses
-- >     Result
data MultiVerb (method :: StdMethod) requestMimeTypes (as :: [Type]) (responses :: Type)

-- | A 'MultiVerb' endpoint with a single response. Ideal to ensure that there can only be one response.
type MultiVerb1 method requestMimeTypes a = MultiVerb method requestMimeTypes '[a] (ResponseType a)

-- | This class is used to convert a handler return type to a union type
-- including all possible responses of a 'MultiVerb' endpoint.
--
-- Any glue code necessary to convert application types to and from the
-- canonical 'Union' type corresponding to a 'MultiVerb' endpoint should be
-- packaged into an 'AsUnion' instance.
--
-- ==== __Example__
-- Let us take the example endpoint from the 'MultiVerb' documentation.
-- There, we derived the 'AsUnion' instance with the help of Generics.
-- The manual way of implementing the instance is:
--
-- > instance AsUnion Responses Result where
-- >   toUnion NegativeNumber = Z (I ())
-- >   toUnion (Even b) = S (Z (I b))
-- >   toUnion (Odd i) = S (S (Z (I i)))
-- >
-- >   fromUnion       (Z (I ())) = NegativeNumber
-- >   fromUnion    (S (Z (I b))) = Even b
-- >   fromUnion (S (S (Z (I i)))) = Odd i
-- >   fromUnion (S (S (S x))) = case x of {}
-- The last 'fromUnion' equation is here to please the pattern checker.
class AsUnion (as :: [Type]) (r :: Type) where
  toUnion :: r -> Union (ResponseTypes as)
  fromUnion :: Union (ResponseTypes as) -> r

-- | Unions can be used directly as handler return types using this trivial
-- instance.
instance rs ~ ResponseTypes as => AsUnion as (Union rs) where
  toUnion = id
  fromUnion = id

-- | A handler with a single response.
instance ResponseType r ~ a => AsUnion '[r] a where
  toUnion = Z . I
  fromUnion = unI . unZ

_foo :: Union '[Int]
_foo = toUnion @'[Respond 200 "test" Int] @Int 3

class InjectAfter as bs where
  injectAfter :: Union bs -> Union (as .++ bs)

instance InjectAfter '[] bs where
  injectAfter = id

instance InjectAfter as bs => InjectAfter (a ': as) bs where
  injectAfter = S . injectAfter @as @bs

class InjectBefore as bs where
  injectBefore :: Union as -> Union (as .++ bs)

instance InjectBefore '[] bs where
  injectBefore x = case x of {}

instance InjectBefore as bs => InjectBefore (a ': as) bs where
  injectBefore (Z x) = Z x
  injectBefore (S x) = S (injectBefore @as @bs x)

eitherToUnion
  :: forall as bs a b
   . (InjectAfter as bs, InjectBefore as bs)
  => (a -> Union as)
  -> (b -> Union bs)
  -> (Either a b -> Union (as .++ bs))
eitherToUnion f _ (Left a) = injectBefore @as @bs (f a)
eitherToUnion _ g (Right b) = injectAfter @as @bs (g b)

class EitherFromUnion as bs where
  eitherFromUnion
    :: (Union as -> a)
    -> (Union bs -> b)
    -> (Union (as .++ bs) -> Either a b)

instance EitherFromUnion '[] bs where
  eitherFromUnion _ g = Right . g

instance EitherFromUnion as bs => EitherFromUnion (a ': as) bs where
  eitherFromUnion f _ (Z x) = Left (f (Z x))
  eitherFromUnion f g (S x) = eitherFromUnion @as @bs (f . S) g x

maybeToUnion
  :: forall as a
   . (InjectAfter as '[()], InjectBefore as '[()])
  => (a -> Union as)
  -> (Maybe a -> Union (as .++ '[()]))
maybeToUnion f (Just a) = injectBefore @as @'[()] (f a)
maybeToUnion _ Nothing = injectAfter @as @'[()] (Z (I ()))

maybeFromUnion
  :: forall as a
   . EitherFromUnion as '[()]
  => (Union as -> a)
  -> (Union (as .++ '[()]) -> Maybe a)
maybeFromUnion f =
  leftToMaybe . eitherFromUnion @as @'[()] f (const (Z (I ())))
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

instance AsConstructor '[a] (Respond code description a) where
  toConstructor x = I x :* Nil
  fromConstructor = unI . hd

instance AsConstructor '[a] (RespondAs (responseContentTypes :: Type) code description a) where
  toConstructor x = I x :* Nil
  fromConstructor = unI . hd

instance AsConstructor '[] (RespondEmpty code description) where
  toConstructor _ = Nil
  fromConstructor _ = ()

instance AsConstructor '[a] (WithHeaders headers a response) where
  toConstructor a = I a :* Nil
  fromConstructor (I a :* Nil) = a

newtype GenericAsConstructor r = GenericAsConstructor r

type instance ResponseType (GenericAsConstructor r) = ResponseType r

instance
  (GSOP.Code (ResponseType r) ~ '[xs], GSOP.Generic (ResponseType r))
  => AsConstructor xs (GenericAsConstructor r)
  where
  toConstructor = unZ . unSOP . GSOP.from
  fromConstructor = GSOP.to . SOP . Z

instance
  (AsConstructor xs r, AsConstructors xss rs)
  => AsConstructors (xs ': xss) (r ': rs)
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
  (GSOP.Code a ~ xss, GSOP.Generic a, AsConstructors xss rs)
  => AsUnion rs (GenericAsUnion rs a)
  where
  toUnion (GenericAsUnion x) = fromSOP @xss @rs (GSOP.from x)
  fromUnion = GenericAsUnion . GSOP.to . toSOP @xss @rs

-- | A handler for a pair of empty responses can be implemented simply by
-- returning a boolean value. The convention is that the "failure" case, normally
-- represented by 'False', corresponds to the /first/ response.
instance
  AsUnion
    '[ RespondEmpty s1 desc1
     , RespondEmpty s2 desc2
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
  {-# OVERLAPPABLE #-}
  (ResponseType r1 ~ (), ResponseType r2 ~ a)
  => AsUnion '[r1, r2] (Maybe a)
  where
  toUnion Nothing = Z (I ())
  toUnion (Just x) = S (Z (I x))

  fromUnion (Z (I ())) = Nothing
  fromUnion (S (Z (I x))) = Just x
  fromUnion (S (S x)) = case x of {}
