{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
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
-- >>> import Servant.Utils.Links
-- >>> import Data.Proxy
-- >>>
-- >>>
-- >>>
-- >>> type Hello = "hello" :> Get '[JSON] Int
-- >>> type Bye   = "bye"   :> QueryParam "name" String :> Delete '[JSON] ()
-- >>> type API   = Hello :<|> Bye
-- >>> let api = Proxy :: Proxy API
--
-- It is possible to generate links that are guaranteed to be within 'API' with
-- 'safeLink'. The first argument to 'safeLink' is a type representing the API
-- you would like to restrict links to. The second argument is the destination
-- endpoint you would like the link to point to, this will need to end with a
-- verb like GET or POST. Further arguments may be required depending on the
-- type of the endpoint. If everything lines up you will get a 'URI' out the
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
-- >>> print (safeLink api hello :: URI)
-- hello
--
-- If the API has an endpoint with parameters then we can generate links with
-- or without those:
--
-- >>> let with = Proxy :: Proxy ("bye" :> QueryParam "name" String :> Delete '[JSON] ())
-- >>> print $ safeLink api with "Hubert"
-- bye?name=Hubert
--
-- >>> let without = Proxy :: Proxy ("bye" :> Delete '[JSON] ())
-- >>> print $ safeLink api without
-- bye
--
-- If you would like create a helper for generating links only within that API,
-- you can partially apply safeLink if you specify a correct type signature
-- like so:
--
-- >>> :set -XConstraintKinds
-- >>> :{
-- >>> let apiLink :: (IsElem endpoint API, HasLink endpoint)
-- >>>             => Proxy endpoint -> MkLink endpoint
-- >>>     apiLink = safeLink api
-- >>> :}
--
-- Attempting to construct a link to an endpoint that does not exist in api
-- will result in a type error like this:
--
-- >>> let bad_link = Proxy :: Proxy ("hello" :> Delete '[JSON] ())
-- >>> safeLink api bad_link
-- ...
--     Could not deduce (Or
--                         (IsElem' (Delete '[JSON] ()) (Get '[JSON] Int))
--                         (IsElem'
--                            ("hello" :> Delete '[JSON] ())
--                            ("bye" :> (QueryParam "name" String :> Delete '[JSON] ()))))
--       arising from a use of ‘safeLink’
--     In the expression: safeLink api bad_link
--     In an equation for ‘it’: it = safeLink api bad_link
--
--  This error is essentially saying that the type family couldn't find
--  bad_link under api after trying the open (but empty) type family
--  `IsElem'` as a last resort.
module Servant.Utils.Links (
  -- * Building and using safe links
  --
  -- | Note that 'URI' is Network.URI.URI from the network-uri package.
    safeLink
  , URI(..)
  -- * Adding custom types
  , HasLink(..)
  , linkURI
  , Link
  , IsElem'
  -- * Illustrative exports
  , IsElem
  , Or
) where

import Data.List
import Data.Proxy ( Proxy(..) )
import Data.Text (Text, unpack)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid ( Monoid(..), (<>) )
#else
import Data.Monoid ( (<>) )
#endif
import Network.URI ( URI(..), escapeURIString, isUnreserved )
import GHC.TypeLits ( KnownSymbol, symbolVal )
import GHC.Exts(Constraint)

import Servant.Common.Text
import Servant.API.Capture ( Capture )
import Servant.API.ReqBody ( ReqBody )
import Servant.API.QueryParam ( QueryParam, QueryParams, QueryFlag )
import Servant.API.MatrixParam ( MatrixParam, MatrixParams, MatrixFlag )
import Servant.API.Header ( Header )
import Servant.API.Get ( Get )
import Servant.API.Post ( Post )
import Servant.API.Put ( Put )
import Servant.API.Delete ( Delete )
import Servant.API.Sub ( type (:>) )
import Servant.API.Raw ( Raw )
import Servant.API.Alternative ( type (:<|>) )

-- | A safe link datatype.
-- The only way of constructing a 'Link' is using 'safeLink', which means any
-- 'Link' is guaranteed to be part of the mentioned API.
data Link = Link
  { _segments :: [String] -- ^ Segments of "foo/bar" would be ["foo", "bar"]
  , _queryParams :: [Param Query]
  } deriving Show

-- | If either a or b produce an empty constraint, produce an empty constraint.
type family Or (a :: Constraint) (b :: Constraint) :: Constraint where
    -- This works because of:
    -- https://ghc.haskell.org/trac/ghc/wiki/NewAxioms/CoincidentOverlap
    Or () b       = ()
    Or a ()       = ()

-- | If both a or b produce an empty constraint, produce an empty constraint.
type family And (a :: Constraint) (b :: Constraint) :: Constraint where
    And () ()     = ()

-- | You may use this type family to tell the type checker that your custom
-- type may be skipped as part of a link. This is useful for things like
-- 'QueryParam' that are optional in a URI and do not affect them if they are
-- omitted.
--
-- >>> data CustomThing
-- >>> type instance IsElem' e (CustomThing :> s) = IsElem e s
--
-- Note that 'IsElem' is called, which will mutually recurse back to `IsElem'`
-- if it exhausts all other options again.
--
-- Once you have written a HasLink instance for CustomThing you are ready to
-- go.
type family IsElem' a s :: Constraint

-- | Closed type family, check if endpoint is within api
type family IsElem endpoint api :: Constraint where
    IsElem e (sa :<|> sb)                   = Or (IsElem e sa) (IsElem e sb)
    IsElem (e :> sa) (e :> sb)              = IsElem sa sb
    IsElem sa (Header x :> sb)              = IsElem sa sb
    IsElem sa (ReqBody y x :> sb)           = IsElem sa sb
    IsElem (Capture z y :> sa) (Capture x y :> sb)
                                            = IsElem sa sb
    IsElem sa (QueryParam x y :> sb)        = IsElem sa sb
    IsElem sa (QueryParams x y :> sb)       = IsElem sa sb
    IsElem sa (QueryFlag x :> sb)           = IsElem sa sb
    IsElem sa (MatrixParam x y :> sb)       = IsElem sa sb
    IsElem sa (MatrixParams x y :> sb)      = IsElem sa sb
    IsElem sa (MatrixFlag x :> sb)          = IsElem sa sb
    IsElem (Get ct typ) (Get ct' typ)       = IsSubList ct ct'
    IsElem (Post ct typ) (Post ct' typ)     = IsSubList ct ct'
    IsElem (Put ct typ) (Put ct' typ)       = IsSubList ct ct'
    IsElem (Delete ct typ) (Delete ct' typ) = IsSubList ct ct'
    IsElem e e                              = ()
    IsElem e a                              = IsElem' e a


type family IsSubList a b :: Constraint where
    IsSubList '[] b          = ()
    IsSubList '[x] (x ': xs) = ()
    IsSubList '[x] (y ': ys) = IsSubList '[x] ys
    IsSubList (x ': xs) y    = IsSubList '[x] y `And` IsSubList xs y

-- Phantom types for Param
data Matrix
data Query

-- | Query/Matrix param
data Param a
    = SingleParam    String Text
    | ArrayElemParam String Text
    | FlagParam      String
  deriving Show

addSegment :: String -> Link -> Link
addSegment seg l = l { _segments = _segments l <> [seg] }

addQueryParam :: Param Query -> Link -> Link
addQueryParam qp l =
    l { _queryParams = _queryParams l <> [qp] }

-- Not particularly efficient for many updates. Something to optimise if it's
-- a problem.
addMatrixParam :: Param Matrix -> Link -> Link
addMatrixParam param l = l { _segments = f (_segments l) }
  where
    f [] = []
    f xs = init xs <> [g (last xs)]
    -- Modify the segment at the "top" of the stack
    g :: String -> String
    g seg =
        case param of
            SingleParam k v    -> seg <> ";" <> k <> "=" <> escape (unpack v)
            ArrayElemParam k v -> seg <> ";" <> k <> "[]=" <> escape (unpack v)
            FlagParam k        -> seg <> ";" <> k

linkURI :: Link -> URI
linkURI (Link segments q_params) =
    URI mempty  -- No scheme (relative)
        Nothing -- Or authority (relative)
        (intercalate "/" segments)
        (makeQueries q_params) mempty
  where
    makeQueries :: [Param Query] -> String
    makeQueries [] = ""
    makeQueries xs =
        "?" <> intercalate "&" (fmap makeQuery xs)

    makeQuery :: Param Query -> String
    makeQuery (ArrayElemParam k v) = escape k <> "[]=" <> escape (unpack v)
    makeQuery (SingleParam k v)    = escape k <> "=" <> escape (unpack v)
    makeQuery (FlagParam k)        = escape k

escape :: String -> String
escape = escapeURIString isUnreserved

-- | Create a valid (by construction) relative URI with query params.
--
-- This function will only typecheck if `endpoint` is part of the API `api`
safeLink
    :: forall endpoint api. (IsElem endpoint api, HasLink endpoint)
    => Proxy api      -- ^ The whole API that this endpoint is a part of
    -> Proxy endpoint -- ^ The API endpoint you would like to point to
    -> MkLink endpoint
safeLink _ endpoint = toLink endpoint (Link mempty mempty)

-- | Construct a toLink for an endpoint.
class HasLink endpoint where
    type MkLink endpoint
    toLink :: Proxy endpoint -- ^ The API endpoint you would like to point to
         -> Link
         -> MkLink endpoint

-- Naked symbol instance
instance (KnownSymbol sym, HasLink sub) => HasLink (sym :> sub) where
    type MkLink (sym :> sub) = MkLink sub
    toLink _ =
        toLink (Proxy :: Proxy sub) . addSegment seg
      where
        seg = symbolVal (Proxy :: Proxy sym)


-- QueryParam instances
instance (KnownSymbol sym, ToText v, HasLink sub)
    => HasLink (QueryParam sym v :> sub) where
    type MkLink (QueryParam sym v :> sub) = v -> MkLink sub
    toLink _ l v =
        toLink (Proxy :: Proxy sub)
             (addQueryParam (SingleParam k (toText v)) l)
      where
        k :: String
        k = symbolVal (Proxy :: Proxy sym)

instance (KnownSymbol sym, ToText v, HasLink sub)
    => HasLink (QueryParams sym v :> sub) where
    type MkLink (QueryParams sym v :> sub) = [v] -> MkLink sub
    toLink _ l =
        toLink (Proxy :: Proxy sub) .
            foldl' (\l' v -> addQueryParam (ArrayElemParam k (toText v)) l') l
      where
        k = symbolVal (Proxy :: Proxy sym)

instance (KnownSymbol sym, HasLink sub)
    => HasLink (QueryFlag sym :> sub) where
    type MkLink (QueryFlag sym :> sub) = Bool -> MkLink sub
    toLink _ l False =
        toLink (Proxy :: Proxy sub) l
    toLink _ l True =
        toLink (Proxy :: Proxy sub) $ addQueryParam (FlagParam k) l
      where
        k = symbolVal (Proxy :: Proxy sym)

-- MatrixParam instances
instance (KnownSymbol sym, ToText v, HasLink sub)
    => HasLink (MatrixParam sym v :> sub) where
    type MkLink (MatrixParam sym v :> sub) = v -> MkLink sub
    toLink _ l v =
        toLink (Proxy :: Proxy sub) $
            addMatrixParam (SingleParam k (toText v)) l
      where
        k = symbolVal (Proxy :: Proxy sym)

instance (KnownSymbol sym, ToText v, HasLink sub)
    => HasLink (MatrixParams sym v :> sub) where
    type MkLink (MatrixParams sym v :> sub) = [v] -> MkLink sub
    toLink _ l =
        toLink (Proxy :: Proxy sub) .
            foldl' (\l' v -> addMatrixParam (ArrayElemParam k (toText v)) l') l
      where
        k = symbolVal (Proxy :: Proxy sym)

instance (KnownSymbol sym, HasLink sub)
    => HasLink (MatrixFlag sym :> sub) where
    type MkLink (MatrixFlag sym :> sub) = Bool -> MkLink sub
    toLink _ l False =
        toLink (Proxy :: Proxy sub) l
    toLink _ l True =
        toLink (Proxy :: Proxy sub) $ addMatrixParam (FlagParam k) l
      where
        k = symbolVal (Proxy :: Proxy sym)

-- Misc instances
instance HasLink sub => HasLink (ReqBody ct a :> sub) where
    type MkLink (ReqBody ct a :> sub) = MkLink sub
    toLink _ = toLink (Proxy :: Proxy sub)

instance (ToText v, HasLink sub)
    => HasLink (Capture sym v :> sub) where
    type MkLink (Capture sym v :> sub) = v -> MkLink sub
    toLink _ l v =
        toLink (Proxy :: Proxy sub) $
            addSegment (escape . unpack $ toText v) l

instance HasLink sub => HasLink (Header sym a :> sub) where
    type MkLink (Header sym a :> sub) = MkLink sub
    toLink _ = toLink (Proxy :: Proxy sub)

-- Verb (terminal) instances
instance HasLink (Get y r) where
    type MkLink (Get y r) = URI
    toLink _ = linkURI

instance HasLink (Post y r) where
    type MkLink (Post y r) = URI
    toLink _ = linkURI

instance HasLink (Put y r) where
    type MkLink (Put y r) = URI
    toLink _ = linkURI

instance HasLink (Delete y r) where
    type MkLink (Delete y r) = URI
    toLink _ = linkURI

instance HasLink Raw where
    type MkLink Raw = URI
    toLink _ = linkURI
