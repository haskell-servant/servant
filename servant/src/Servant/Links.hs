{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE QuantifiedConstraints    #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-# OPTIONS_HADDOCK not-home        #-}

-- | Type safe generation of internal links.
--
-- Given an API with a few endpoints:
--
-- >>> :set -XDataKinds -XTypeFamilies -XTypeOperators
-- >>> import Servant.API
-- >>> import Servant.Links
-- >>> import Web.HttpApiData (toUrlPiece)
-- >>> import Data.Proxy
-- >>>
-- >>> type Hello = "hello" :> Get '[JSON] Int
-- >>> type Bye   = "bye"   :> QueryParam "name" String :> Delete '[JSON] NoContent
-- >>> type API   = Hello :<|> Bye
-- >>> let api = Proxy :: Proxy API
--
-- It is possible to generate links that are guaranteed to be within 'API' with
-- 'safeLink'. The first argument to 'safeLink' is a type representing the API
-- you would like to restrict links to. The second argument is the destination
-- endpoint you would like the link to point to, this will need to end with a
-- verb like GET or POST. Further arguments may be required depending on the
-- type of the endpoint. If everything lines up you will get a 'Link' out the
-- other end.
--
-- You may omit 'QueryParam's and the like should you not want to provide them,
-- but types which form part of the URL path like 'Capture' must be included.
-- The reason you may want to omit 'QueryParam's is that safeLink is a bit
-- magical: if parameters are included that could take input it will return a
-- function that accepts that input and generates a link. This is best shown
-- with an example. Here, a link is generated with no parameters:
--
-- >>> let hello = Proxy :: Proxy ("hello" :> Get '[JSON] Int)
-- >>> toUrlPiece (safeLink api hello :: Link)
-- "hello"
--
-- If the API has an endpoint with parameters then we can generate links with
-- or without those:
--
-- >>> let with = Proxy :: Proxy ("bye" :> QueryParam "name" String :> Delete '[JSON] NoContent)
-- >>> toUrlPiece $ safeLink api with (Just "Hubert")
-- "bye?name=Hubert"
--
-- >>> let without = Proxy :: Proxy ("bye" :> Delete '[JSON] NoContent)
-- >>> toUrlPiece $ safeLink api without
-- "bye"
--
-- If you would like to create a helper for generating links only within that API,
-- you can partially apply safeLink if you specify a correct type signature
-- like so:
--
-- >>> :set -XConstraintKinds
-- >>> :{
-- >>> let apiLink :: (IsElem endpoint API, HasLink endpoint)
-- >>>             => Proxy endpoint -> MkLink endpoint Link
-- >>>     apiLink = safeLink api
-- >>> :}
--
-- `safeLink'` allows you to specialise the output:
--
-- >>> safeLink' toUrlPiece api without
-- "bye"
--
-- >>> :{
-- >>> let apiTextLink :: (IsElem endpoint API, HasLink endpoint)
-- >>>                  => Proxy endpoint -> MkLink endpoint Text
-- >>>     apiTextLink = safeLink' toUrlPiece api
-- >>> :}
--
-- >>> apiTextLink without
-- "bye"
--
-- Attempting to construct a link to an endpoint that does not exist in api
-- will result in a type error like this:
--
-- >>> let bad_link = Proxy :: Proxy ("hello" :> Delete '[JSON] NoContent)
-- >>> safeLink api bad_link
-- ...
-- ...Could not deduce...
-- ...
--
--  This error is essentially saying that the type family couldn't find
--  bad_link under api after trying the open (but empty) type family
--  `IsElem'` as a last resort.
--
--  @since 0.14.1
module Servant.Links (
  module Servant.API.TypeLevel,

  -- * Building and using safe links
  --
  -- | Note that 'URI' is from the "Network.URI" module in the @network-uri@ package.
    safeLink
  , safeLink'
  , allLinks
  , allLinks'
  , URI(..)
  -- * Generics
  , AsLink
  , fieldLink
  , fieldLink'
  , allFieldLinks
  , allFieldLinks'
  -- * Adding custom types
  , HasLink(..)
  , Link
  , linkURI
  , linkURI'
  , LinkArrayElementStyle (..)
  -- ** Link accessors
  , Param (..)
  , linkSegments
  , linkQueryParams
  , linkFragment
) where

import           Data.List
import           Data.Constraint
import           Data.Proxy
                 (Proxy (..))
import           Data.Singletons.Bool
                 (SBool (..), SBoolI (..))
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as TE
import           Data.Type.Bool
                 (If)
import           GHC.TypeLits
                 (KnownSymbol, TypeError, symbolVal)
import           Network.URI
                 (URI (..), escapeURIString, isUnreserved)
import           Prelude ()
import           Prelude.Compat

import           Servant.API.Alternative
                 ((:<|>) ((:<|>)))
import           Servant.API.BasicAuth
                 (BasicAuth)
import           Servant.API.Capture
                 (Capture', CaptureAll)
import           Servant.API.Description
                 (Description, Summary)
import           Servant.API.Empty
                 (EmptyAPI (..))
import           Servant.API.Experimental.Auth
                 (AuthProtect)
import           Servant.API.Fragment
                 (Fragment)
import           Servant.API.Generic
import           Servant.API.Header
                 (Header')
import           Servant.API.HttpVersion
                 (HttpVersion)
import           Servant.API.IsSecure
                 (IsSecure)
import           Servant.API.Modifiers
                 (FoldRequired)
import           Servant.API.NamedRoutes
                 (NamedRoutes)
import           Servant.API.QueryParam
                 (QueryFlag, QueryParam', QueryParams)
import           Servant.API.Raw
                 (Raw)
import           Servant.API.RemoteHost
                 (RemoteHost)
import           Servant.API.ReqBody
                 (ReqBody')
import           Servant.API.Stream
                 (Stream, StreamBody')
import           Servant.API.Sub
                 (type (:>))
import           Servant.API.TypeErrors
import           Servant.API.TypeLevel
import           Servant.API.UVerb
import           Servant.API.Vault
                 (Vault)
import           Servant.API.Verbs
                 (Verb, NoContentVerb)
import           Servant.API.WithNamedContext
                 (WithNamedContext)
import           Web.HttpApiData

-- | A safe link datatype.
-- The only way of constructing a 'Link' is using 'safeLink', which means any
-- 'Link' is guaranteed to be part of the mentioned API.
data Link = Link
  { _segments    :: [Escaped]
  , _queryParams :: [Param]
  , _fragment    :: Fragment'
  } deriving Show

newtype Escaped = Escaped String

type Fragment' = Maybe String

escaped :: String -> Escaped
escaped = Escaped . escapeURIString isUnreserved

getEscaped :: Escaped -> String
getEscaped (Escaped s) = s

instance Show Escaped where
    showsPrec d (Escaped s) = showsPrec d s
    show (Escaped s)        = show s

linkSegments :: Link -> [String]
linkSegments = map getEscaped . _segments

linkQueryParams :: Link -> [Param]
linkQueryParams = _queryParams

linkFragment :: Link -> Fragment'
linkFragment = _fragment

instance ToHttpApiData Link where
    toHeader   = TE.encodeUtf8 . toUrlPiece
    toUrlPiece l =
        let uri = linkURI l
        in Text.pack $ uriPath uri ++ uriQuery uri ++ uriFragment uri

-- | Query parameter.
data Param
    = SingleParam    String Text.Text
    | ArrayElemParam String Text.Text
    | FlagParam      String
  deriving Show

addSegment :: Escaped -> Link -> Link
addSegment seg l = l { _segments = _segments l <> [seg] }

addQueryParam :: Param -> Link -> Link
addQueryParam qp l =
    l { _queryParams = _queryParams l <> [qp] }

addFragment :: Fragment' -> Link -> Link
addFragment fr l = l { _fragment = fr }

-- | Transform 'Link' into 'URI'.
--
-- >>> type API = "something" :> Get '[JSON] Int
-- >>> linkURI $ safeLink (Proxy :: Proxy API) (Proxy :: Proxy API)
-- something
--
-- >>> type API = "sum" :> QueryParams "x" Int :> Get '[JSON] Int
-- >>> linkURI $ safeLink (Proxy :: Proxy API) (Proxy :: Proxy API) [1, 2, 3]
-- sum?x[]=1&x[]=2&x[]=3
--
-- >>> type API = "foo/bar" :> Get '[JSON] Int
-- >>> linkURI $ safeLink (Proxy :: Proxy API) (Proxy :: Proxy API)
-- foo%2Fbar
--
-- >>> type SomeRoute = "abc" :> Capture "email" String :> Put '[JSON] ()
-- >>> let someRoute = Proxy :: Proxy SomeRoute
-- >>> safeLink someRoute someRoute "test@example.com"
-- Link {_segments = ["abc","test%40example.com"], _queryParams = [], _fragment = Nothing}
--
-- >>> linkURI $ safeLink someRoute someRoute "test@example.com"
-- abc/test%40example.com
--
linkURI :: Link -> URI
linkURI = linkURI' LinkArrayElementBracket

-- | How to encode array query elements.
data LinkArrayElementStyle
    = LinkArrayElementBracket  -- ^ @foo[]=1&foo[]=2@
    | LinkArrayElementPlain    -- ^ @foo=1&foo=2@
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Configurable 'linkURI'.
--
-- >>> type API = "sum" :> QueryParams "x" Int :> Get '[JSON] Int
-- >>> linkURI' LinkArrayElementBracket $ safeLink (Proxy :: Proxy API) (Proxy :: Proxy API) [1, 2, 3]
-- sum?x[]=1&x[]=2&x[]=3
--
-- >>> linkURI' LinkArrayElementPlain $ safeLink (Proxy :: Proxy API) (Proxy :: Proxy API) [1, 2, 3]
-- sum?x=1&x=2&x=3
--
linkURI' :: LinkArrayElementStyle -> Link -> URI
linkURI' addBrackets (Link segments q_params mfragment) =
    URI mempty  -- No scheme (relative)
        Nothing -- Or authority (relative)
        (intercalate "/" $ map getEscaped segments)
        (makeQueries q_params)
        (makeFragment mfragment)
  where
    makeQueries :: [Param] -> String
    makeQueries [] = ""
    makeQueries xs =
        "?" <> intercalate "&" (fmap makeQuery xs)

    makeQuery :: Param -> String
    makeQuery (ArrayElemParam k v) = escape k <> style <> escape (Text.unpack v)
    makeQuery (SingleParam k v)    = escape k <> "=" <> escape (Text.unpack v)
    makeQuery (FlagParam k)        = escape k

    makeFragment :: Fragment' -> String
    makeFragment Nothing = ""
    makeFragment (Just fr) = "#" <> escape fr

    style = case addBrackets of
        LinkArrayElementBracket -> "[]="
        LinkArrayElementPlain -> "="

escape :: String -> String
escape = escapeURIString isUnreserved

-- | Create a valid (by construction) relative URI with query params.
--
-- This function will only typecheck if `endpoint` is part of the API `api`
safeLink
    :: forall endpoint api. (IsElem endpoint api, HasLink endpoint)
    => Proxy api      -- ^ The whole API that this endpoint is a part of
    -> Proxy endpoint -- ^ The API endpoint you would like to point to
    -> MkLink endpoint Link
safeLink = safeLink' id

-- | More general 'safeLink'.
--
safeLink'
    :: forall endpoint api a. (IsElem endpoint api, HasLink endpoint)
    => (Link -> a)
    -> Proxy api      -- ^ The whole API that this endpoint is a part of
    -> Proxy endpoint -- ^ The API endpoint you would like to point to
    -> MkLink endpoint a
safeLink' toA _ endpoint = toLink toA endpoint (Link mempty mempty mempty)

-- | Create all links in an API.
--
-- Note that the @api@ type must be restricted to the endpoints that have
-- valid links to them.
--
-- >>> type API = "foo" :> Capture "name" Text :> Get '[JSON] Text :<|> "bar" :> Capture "name" Int :> Get '[JSON] Double
-- >>> let fooLink :<|> barLink = allLinks (Proxy :: Proxy API)
-- >>> :t fooLink
-- fooLink :: Text -> Link
-- >>> :t barLink
-- barLink :: Int -> Link
--
-- Note: nested APIs don't work well with this approach
--
-- >>> :kind! MkLink (Capture "nest" Char :> (Capture "x" Int :> Get '[JSON] Int :<|> Capture "y" Double :> Get '[JSON] Double)) Link
-- MkLink (Capture "nest" Char :> (Capture "x" Int :> Get '[JSON] Int :<|> Capture "y" Double :> Get '[JSON] Double)) Link :: *
-- = Char -> (Int -> Link) :<|> (Double -> Link)
allLinks
    :: forall api. HasLink api
    => Proxy api
    -> MkLink api Link
allLinks = allLinks' id

-- | More general 'allLinks'. See `safeLink'`.
allLinks'
    :: forall api a. HasLink api
    => (Link -> a)
    -> Proxy api
    -> MkLink api a
allLinks' toA api = toLink toA api (Link mempty mempty mempty)

-------------------------------------------------------------------------------
-- Generics
-------------------------------------------------------------------------------

-- | Given an API record field, create a link for that route. Only the field's
-- type is used.
--
-- @
-- data Record route = Record
--     { _get :: route :- Capture "id" Int :> Get '[JSON] String
--     , _put :: route :- ReqBody '[JSON] Int :> Put '[JSON] Bool
--     }
--   deriving ('Generic')
--
-- getLink :: Int -> Link
-- getLink = 'fieldLink' _get
-- @
--
-- @since 0.14.1
fieldLink
    :: ( IsElem endpoint (ToServantApi routes), HasLink endpoint
       , GenericServant routes AsApi
       )
    => (routes AsApi -> endpoint)
    -> MkLink endpoint Link
fieldLink = fieldLink' id

-- | More general version of 'fieldLink'
--
-- @since 0.14.1
fieldLink'
    :: forall routes endpoint a.
       ( IsElem endpoint (ToServantApi routes), HasLink endpoint
       , GenericServant routes AsApi
       )
    => (Link -> a)
    -> (routes AsApi -> endpoint)
    -> MkLink endpoint a
fieldLink' toA _ = safeLink' toA (genericApi (Proxy :: Proxy routes)) (Proxy :: Proxy endpoint)

-- | A type that specifies that an API record contains a set of links.
--
-- @since 0.14.1
data AsLink (a :: *)
instance GenericMode (AsLink a) where
    type (AsLink a) :- api = MkLink api a

-- | Get all links as a record.
--
-- @since 0.14.1
allFieldLinks
    :: ( HasLink (ToServantApi routes)
       , GenericServant routes (AsLink Link)
       , ToServant routes (AsLink Link) ~ MkLink (ToServantApi routes) Link
       )
    => routes (AsLink Link)
allFieldLinks = allFieldLinks' id

-- | More general version of 'allFieldLinks'.
--
-- @since 0.14.1
allFieldLinks'
    :: forall routes a.
       ( HasLink (ToServantApi routes)
       , GenericServant routes (AsLink a)
       , ToServant routes (AsLink a) ~ MkLink (ToServantApi routes) a
       )
    => (Link -> a)
    -> routes (AsLink a)
allFieldLinks' toA
    = fromServant
    $ allLinks' toA (Proxy :: Proxy (ToServantApi routes))

-------------------------------------------------------------------------------
-- HasLink
-------------------------------------------------------------------------------

-- | Construct a toLink for an endpoint.
class HasLink endpoint where
    type MkLink endpoint (a :: *)
    toLink
        :: (Link -> a)
        -> Proxy endpoint -- ^ The API endpoint you would like to point to
        -> Link
        -> MkLink endpoint a

-- Naked symbol instance
instance (KnownSymbol sym, HasLink sub) => HasLink (sym :> sub) where
    type MkLink (sym :> sub) a = MkLink sub a
    toLink toA _ =
        toLink toA (Proxy :: Proxy sub) . addSegment (escaped seg)
      where
        seg = symbolVal (Proxy :: Proxy sym)

-- QueryParam instances
instance (KnownSymbol sym, ToHttpApiData v, HasLink sub, SBoolI (FoldRequired mods))
    => HasLink (QueryParam' mods sym v :> sub)
  where
    type MkLink (QueryParam' mods sym v :> sub) a = If (FoldRequired mods) v (Maybe v) -> MkLink sub a
    toLink toA _ l mv =
        toLink toA (Proxy :: Proxy sub) $
            case sbool :: SBool (FoldRequired mods) of
                STrue  -> (addQueryParam . SingleParam k . toQueryParam) mv l
                SFalse -> maybe id (addQueryParam . SingleParam k . toQueryParam) mv l
      where
        k :: String
        k = symbolVal (Proxy :: Proxy sym)

instance (KnownSymbol sym, ToHttpApiData v, HasLink sub)
    => HasLink (QueryParams sym v :> sub)
  where
    type MkLink (QueryParams sym v :> sub) a = [v] -> MkLink sub a
    toLink toA _ l =
        toLink toA (Proxy :: Proxy sub) .
            foldl' (\l' v -> addQueryParam (ArrayElemParam k (toQueryParam v)) l') l
      where
        k = symbolVal (Proxy :: Proxy sym)

instance (KnownSymbol sym, HasLink sub)
    => HasLink (QueryFlag sym :> sub)
  where
    type MkLink (QueryFlag sym :> sub) a = Bool -> MkLink sub a
    toLink toA  _ l False =
        toLink toA (Proxy :: Proxy sub) l
    toLink toA _ l True =
        toLink toA (Proxy :: Proxy sub) $ addQueryParam (FlagParam k) l
      where
        k = symbolVal (Proxy :: Proxy sym)

-- :<|> instance - Generate all links at once
instance (HasLink a, HasLink b) => HasLink (a :<|> b) where
    type MkLink (a :<|> b) r = MkLink a r :<|> MkLink b r
    toLink toA _ l = toLink toA (Proxy :: Proxy a) l :<|> toLink toA (Proxy :: Proxy b) l

-- Misc instances
instance HasLink sub => HasLink (ReqBody' mods ct a :> sub) where
    type MkLink (ReqBody' mods ct a :> sub) r = MkLink sub r
    toLink toA _ = toLink toA (Proxy :: Proxy sub)

instance HasLink sub => HasLink (StreamBody' mods framing ct a :> sub) where
    type MkLink (StreamBody' mods framing ct a :> sub) r = MkLink sub r
    toLink toA _ = toLink toA (Proxy :: Proxy sub)

instance (ToHttpApiData v, HasLink sub)
    => HasLink (Capture' mods sym v :> sub)
  where
    type MkLink (Capture' mods sym v :> sub) a = v -> MkLink sub a
    toLink toA _ l v =
        toLink toA (Proxy :: Proxy sub) $
            addSegment (escaped . Text.unpack $ toUrlPiece v) l

instance (ToHttpApiData v, HasLink sub)
    => HasLink (CaptureAll sym v :> sub)
  where
    type MkLink (CaptureAll sym v :> sub) a = [v] -> MkLink sub a
    toLink toA _ l vs = toLink toA (Proxy :: Proxy sub) $
        foldl' (flip $ addSegment . escaped . Text.unpack . toUrlPiece) l vs

instance HasLink sub => HasLink (Header' mods sym (a :: *) :> sub) where
    type MkLink (Header' mods sym a :> sub) r = MkLink sub r
    toLink = simpleToLink (Proxy :: Proxy sub)

instance HasLink sub => HasLink (Vault :> sub) where
    type MkLink (Vault :> sub) a = MkLink sub a
    toLink = simpleToLink (Proxy :: Proxy sub)

instance HasLink sub => HasLink (Description s :> sub) where
    type MkLink (Description s :> sub) a = MkLink sub a
    toLink = simpleToLink (Proxy :: Proxy sub)

instance HasLink sub => HasLink (Summary s :> sub) where
    type MkLink (Summary s :> sub) a = MkLink sub a
    toLink = simpleToLink (Proxy :: Proxy sub)

instance HasLink sub => HasLink (HttpVersion :> sub) where
    type MkLink (HttpVersion:> sub) a = MkLink sub a
    toLink = simpleToLink (Proxy :: Proxy sub)

instance HasLink sub => HasLink (IsSecure :> sub) where
    type MkLink (IsSecure :> sub) a = MkLink sub a
    toLink = simpleToLink (Proxy :: Proxy sub)

instance HasLink sub => HasLink (WithNamedContext name context sub) where
    type MkLink (WithNamedContext name context sub) a = MkLink sub a
    toLink toA _ = toLink toA (Proxy :: Proxy sub)

instance HasLink sub => HasLink (RemoteHost :> sub) where
    type MkLink (RemoteHost :> sub) a = MkLink sub a
    toLink = simpleToLink (Proxy :: Proxy sub)

instance HasLink sub => HasLink (BasicAuth realm a :> sub) where
    type MkLink (BasicAuth realm a :> sub) r = MkLink sub r
    toLink = simpleToLink (Proxy :: Proxy sub)

instance HasLink EmptyAPI where
    type MkLink EmptyAPI a = EmptyAPI
    toLink _ _ _ = EmptyAPI

-- Verb (terminal) instances
instance HasLink (Verb m s ct a) where
    type MkLink (Verb m s ct a) r = r
    toLink toA _ = toA

instance HasLink (NoContentVerb m) where
    type MkLink (NoContentVerb m) r = r
    toLink toA _ = toA

instance HasLink Raw where
    type MkLink Raw a = a
    toLink toA _ = toA

instance HasLink (Stream m status fr ct a) where
    type MkLink (Stream m status fr ct a) r = r
    toLink toA _ = toA

-- UVerb instances
instance HasLink (UVerb m ct a) where
    type MkLink (UVerb m ct a) r = r
    toLink toA _ = toA
-- Instance for NamedRoutes combinator

type GLinkConstraints routes a =
  ( MkLink (ToServant routes AsApi) a ~ ToServant routes (AsLink a)
  , GenericServant routes (AsLink a)
  )

class GLink (routes :: * -> *) (a :: *) where
  gLinkProof :: Dict (GLinkConstraints routes a)

instance GLinkConstraints routes a => GLink routes a where
  gLinkProof = Dict

instance
  ( HasLink (ToServantApi routes)
  , forall a. GLink routes a
  ) => HasLink (NamedRoutes routes) where

  type MkLink (NamedRoutes routes) a = routes (AsLink a)

  toLink
    :: forall a. (Link -> a)
    -> Proxy (NamedRoutes routes)
    -> Link
    -> routes (AsLink a)

  toLink toA _ l = case gLinkProof @routes @a of
    Dict -> fromServant $ toLink toA (Proxy @(ToServantApi routes)) l

-- AuthProtext instances
instance HasLink sub => HasLink (AuthProtect tag :> sub) where
  type MkLink (AuthProtect tag :> sub) a = MkLink sub a
  toLink = simpleToLink (Proxy :: Proxy sub)

instance (HasLink sub, ToHttpApiData v)
    => HasLink (Fragment v :> sub) where
  type MkLink (Fragment v :> sub) a = v -> MkLink sub a
  toLink toA _ l mv =
      toLink toA (Proxy :: Proxy sub) $
         addFragment ((Just . Text.unpack . toQueryParam) mv) l

-- | Helper for implementing 'toLink' for combinators not affecting link
-- structure.
simpleToLink
    :: forall sub a combinator.
       (HasLink sub, MkLink sub a ~ MkLink (combinator :> sub) a)
    => Proxy sub
    -> (Link -> a)
    -> Proxy (combinator :> sub)
    -> Link
    -> MkLink (combinator :> sub) a
simpleToLink _ toA _ = toLink toA (Proxy :: Proxy sub)


-- $setup
-- >>> import Servant.API
-- >>> import Data.Text (Text)

-- Erroring instance for 'HasLink' when a combinator is not fully applied
instance TypeError (PartialApplication HasLink arr) => HasLink ((arr :: a -> b) :> sub)
  where
    type MkLink (arr :> sub) _ = TypeError (PartialApplication HasLink arr)
    toLink = error "unreachable"

-- Erroring instances for 'HasLink' for unknown API combinators
instance {-# OVERLAPPABLE #-} TypeError (NoInstanceForSub HasLink ty) => HasLink (ty :> sub)

instance {-# OVERLAPPABLE #-} TypeError (NoInstanceFor (HasLink api)) => HasLink api
