{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | This module provides facilities for adding headers to a response.
--
-- >>> let headerVal = addHeader "some-url" 5 :: Headers '[Header "Location" String] Int
--
-- The value is added to the header specified by the type (@Location@ in the
-- example above).
module Servant.API.ResponseHeaders
  ( Headers (..)
  , ResponseHeader (..)
  , AddHeader
  , addHeader
  , addHeader'
  , noHeader
  , noHeader'
  , HasResponseHeader
  , lookupResponseHeader
  , BuildHeadersTo (buildHeadersTo)
  , GetHeaders (getHeaders)
  , GetHeaders'
  , HeaderValMap
  , HList (..)
  )
where

import Control.DeepSeq (NFData (..))
import Data.ByteString.Char8 as BS (ByteString, pack)
import qualified Data.CaseInsensitive as CI
import Data.Kind (Type)
import qualified Data.List as L
import Data.Proxy
import qualified Data.SOP.BasicFunctors as SOP
import qualified Data.SOP.NS as SOP
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Network.HTTP.Types.Header as HTTP
import Web.HttpApiData (FromHttpApiData, ToHttpApiData, parseHeader, toHeader)

import Servant.API.Header (Header')
import Servant.API.Modifiers (Optional, Strict)
import Servant.API.UVerb.Union

-- | Response Header objects. You should never need to construct one directly.
-- Instead, use 'addOptionalHeader'.
data Headers ls a = Headers
  { getResponse :: a
  -- ^ The underlying value of a 'Headers'
  , getHeadersHList :: HList ls
  -- ^ HList of headers.
  }
  deriving (Functor)

instance (NFData a, NFDataHList ls) => NFData (Headers ls a) where
  rnf (Headers x hdrs) = rnf x `seq` rnf hdrs

data ResponseHeader (sym :: Symbol) a
  = Header a
  | MissingHeader
  | UndecodableHeader ByteString
  deriving (Eq, Functor, Show, Typeable)

instance NFData a => NFData (ResponseHeader sym a) where
  rnf MissingHeader = ()
  rnf (UndecodableHeader bs) = rnf bs
  rnf (Header x) = rnf x

data HList a where
  HNil :: HList '[]
  HCons :: ResponseHeader h x -> HList xs -> HList (Header' mods h x ': xs)

class NFDataHList xs where rnfHList :: HList xs -> ()

instance NFDataHList '[] where rnfHList HNil = ()

instance (NFData x, NFDataHList xs, y ~ Header' mods h x) => NFDataHList (y ': xs) where
  rnfHList (HCons h xs) = rnf h `seq` rnfHList xs

instance NFDataHList xs => NFData (HList xs) where
  rnf = rnfHList

type family HeaderValMap (f :: Type -> Type) (xs :: [Type]) where
  HeaderValMap f '[] = '[]
  HeaderValMap f (Header' mods h x ': xs) = Header' mods h (f x) ': HeaderValMap f xs

class BuildHeadersTo hs where
  buildHeadersTo :: [HTTP.Header] -> HList hs

instance {-# OVERLAPPING #-} BuildHeadersTo '[] where
  buildHeadersTo _ = HNil

-- The current implementation does not manipulate HTTP header field lines in any way,
-- like merging field lines with the same field name in a single line.
instance
  {-# OVERLAPPABLE #-}
  (BuildHeadersTo xs, FromHttpApiData v, KnownSymbol h)
  => BuildHeadersTo (Header' mods h v ': xs)
  where
  buildHeadersTo headers = case L.find wantedHeader headers of
    Nothing -> MissingHeader `HCons` buildHeadersTo headers
    Just header@(_, val) -> case parseHeader val of
      Left _err -> UndecodableHeader val `HCons` buildHeadersTo (L.delete header headers)
      Right h -> Header h `HCons` buildHeadersTo (L.delete header headers)
    where
      wantedHeader (h, _) = h == wantedHeaderName
      wantedHeaderName = CI.mk . pack $ symbolVal (Proxy :: Proxy h)

-- * Getting headers

class GetHeaders ls where
  getHeaders :: ls -> [HTTP.Header]

-- | Auxiliary class for @'GetHeaders' ('HList' hs)@ instance
class GetHeadersFromHList hs where
  getHeadersFromHList :: HList hs -> [HTTP.Header]

instance GetHeadersFromHList hs => GetHeaders (HList hs) where
  getHeaders = getHeadersFromHList

instance GetHeadersFromHList '[] where
  getHeadersFromHList _ = []

instance
  (GetHeadersFromHList xs, KnownSymbol h, ToHttpApiData x)
  => GetHeadersFromHList (Header' mods h x ': xs)
  where
  getHeadersFromHList hdrs = case hdrs of
    Header val `HCons` rest -> (headerName, toHeader val) : getHeadersFromHList rest
    UndecodableHeader h `HCons` rest -> (headerName, h) : getHeadersFromHList rest
    MissingHeader `HCons` rest -> getHeadersFromHList rest
    where
      headerName = CI.mk . pack $ symbolVal (Proxy :: Proxy h)

-- | Auxiliary class for @'GetHeaders' ('Headers' hs a)@ instance
class GetHeaders' hs where
  getHeaders' :: Headers hs a -> [HTTP.Header]

instance GetHeaders' hs => GetHeaders (Headers hs a) where
  getHeaders = getHeaders'

-- | This instance is an optimisation
instance GetHeaders' '[] where
  getHeaders' _ = []

instance
  (GetHeadersFromHList rest, KnownSymbol h, ToHttpApiData v)
  => GetHeaders' (Header' mods h v ': rest)
  where
  getHeaders' hs = getHeadersFromHList $ getHeadersHList hs

-- * Adding headers

-- We need all these fundeps to save type inference
class
  AddHeader (mods :: [Type]) h v orig new
    | mods h v orig -> new
    , new -> mods
    , new -> h
    , new -> v
    , new -> orig
  where
  addOptionalHeader
    :: ResponseHeader h v
    -> orig
    -> new
    -- ^ N.B.: The same header can't be added multiple times

-- In this instance, we add a Header on top of something that is already decorated with some headers
instance
  {-# OVERLAPPING #-}
  (KnownSymbol h, ToHttpApiData v)
  => AddHeader mods h v (Headers (fst ': rest) a) (Headers (Header' mods h v ': fst ': rest) a)
  where
  addOptionalHeader hdr (Headers resp heads) = Headers resp (HCons hdr heads)

-- In this instance, 'a' parameter is decorated with a Header.
instance
  {-# OVERLAPPABLE #-}
  (KnownSymbol h, ToHttpApiData v, new ~ Headers '[Header' mods h v] a)
  => AddHeader mods h v a new
  where
  addOptionalHeader hdr resp = Headers resp (HCons hdr HNil)

-- Instances to decorate all responses in a 'Union' with headers. The functional
-- dependencies force us to consider singleton lists as the base case in the
-- recursion (it is impossible to determine h and v otherwise from old / new
-- responses if the list is empty).
instance AddHeader mods h v old new => AddHeader mods h v (Union '[old]) (Union '[new]) where
  addOptionalHeader hdr resp =
    SOP.Z $ SOP.I $ addOptionalHeader hdr $ SOP.unI $ SOP.unZ resp

instance
  ( AddHeader mods h v (Union oldrest) (Union newrest)
  , AddHeader mods h v old new
  , -- This ensures that the remainder of the response list is _not_ empty
    -- It is necessary to prevent the two instances for union types from
    -- overlapping.

    newrest ~ (b ': bs)
  , oldrest ~ (a ': as)
  )
  => AddHeader mods h v (Union (old ': (a ': as))) (Union (new ': (b ': bs)))
  where
  addOptionalHeader hdr resp = case resp of
    SOP.Z (SOP.I rHead) -> SOP.Z $ SOP.I $ addOptionalHeader hdr rHead
    SOP.S rOthers -> SOP.S $ addOptionalHeader hdr rOthers

-- | @addHeader@ adds a header to a response. Note that it changes the type of
-- the value in the following ways:
--
--   1. A simple value is wrapped in "Headers '[hdr]":
--
-- >>> let example0 = addHeader 5 "hi" :: Headers '[Header "someheader" Int] String;
-- >>> getHeaders example0
-- [("someheader","5")]
--
--   2. A value that already has a header has its new header *prepended* to the
--   existing list:
--
-- >>> let example1 = addHeader 5 "hi" :: Headers '[Header "someheader" Int] String;
-- >>> let example2 = addHeader True example1 :: Headers '[Header "1st" Bool, Header "someheader" Int] String
-- >>> getHeaders example2
-- [("1st","true"),("someheader","5")]
--
-- Note that while in your handlers type annotations are not required, since
-- the type can be inferred from the API type, in other cases you may find
-- yourself needing to add annotations.
addHeader :: AddHeader '[Optional, Strict] h v orig new => v -> orig -> new
addHeader = addOptionalHeader . Header

-- | Same as 'addHeader' but works with `Header'`, so it's possible to use any @mods@.
addHeader' :: AddHeader mods h v orig new => v -> orig -> new
addHeader' = addOptionalHeader . Header

-- | Deliberately do not add a header to a value.
--
-- >>> let example1 = noHeader "hi" :: Headers '[Header "someheader" Int] String
-- >>> getHeaders example1
-- []
noHeader :: AddHeader '[Optional, Strict] h v orig new => orig -> new
noHeader = addOptionalHeader MissingHeader

-- | Same as 'noHeader' but works with `Header'`, so it's possible to use any @mods@.
noHeader' :: AddHeader mods h v orig new => orig -> new
noHeader' = addOptionalHeader MissingHeader

class HasResponseHeader h a headers where
  hlistLookupHeader :: HList headers -> ResponseHeader h a

instance {-# OVERLAPPING #-} HasResponseHeader h a (Header' mods h a ': rest) where
  hlistLookupHeader (HCons ha _) = ha

instance {-# OVERLAPPABLE #-} HasResponseHeader h a rest => HasResponseHeader h a (first ': rest) where
  hlistLookupHeader (HCons _ hs) = hlistLookupHeader hs

-- | Look up a specific ResponseHeader,
-- without having to know what position it is in the HList.
--
-- >>> let example1 = addHeader 5 "hi" :: Headers '[Header "someheader" Int] String
-- >>> let example2 = addHeader True example1 :: Headers '[Header "1st" Bool, Header "someheader" Int] String
-- >>> lookupResponseHeader example2 :: ResponseHeader "someheader" Int
-- Header 5
--
-- >>> lookupResponseHeader example2 :: ResponseHeader "1st" Bool
-- Header True
--
-- Usage of this function relies on an explicit type annotation of the header to be looked up.
-- This can be done with type annotations on the result, or with an explicit type application.
-- In this example, the type of header value is determined by the type-inference,
-- we only specify the name of the header:
--
-- >>> :set -XTypeApplications
-- >>> case lookupResponseHeader @"1st" example2 of { Header b -> b ; _ -> False }
-- True
--
-- @since 0.15
lookupResponseHeader
  :: HasResponseHeader h a headers
  => Headers headers r
  -> ResponseHeader h a
lookupResponseHeader = hlistLookupHeader . getHeadersHList

-- $setup
-- >>> :set -XFlexibleContexts
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
