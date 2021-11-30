{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_HADDOCK not-home        #-}

-- | This module provides facilities for adding headers to a response.
--
-- >>> let headerVal = addHeader "some-url" 5 :: Headers '[Header "Location" String] Int
--
-- The value is added to the header specified by the type (@Location@ in the
-- example above).
module Servant.API.ResponseHeaders
    ( Headers(..)
    , ResponseHeader (..)
    , AddHeader
    , addHeader
    , noHeader
    , HasResponseHeader
    , lookupResponseHeader
    , BuildHeadersTo(buildHeadersTo)
    , GetHeaders(getHeaders)
    , GetHeaders'
    , HeaderValMap
    , HList(..)
    ) where

import           Control.DeepSeq
                 (NFData (..))
import           Data.ByteString.Char8     as BS
                 (ByteString, init, pack, unlines)
import qualified Data.CaseInsensitive      as CI
import           Data.Proxy
import           Data.Typeable
                 (Typeable)
import           GHC.TypeLits
                 (KnownSymbol, Symbol, symbolVal)
import qualified Network.HTTP.Types.Header as HTTP
import           Web.HttpApiData
                 (FromHttpApiData, ToHttpApiData, parseHeader, toHeader)

import           Prelude ()
import           Prelude.Compat
import           Servant.API.Header
                 (Header)

-- | Response Header objects. You should never need to construct one directly.
-- Instead, use 'addOptionalHeader'.
data Headers ls a = Headers { getResponse :: a
                            -- ^ The underlying value of a 'Headers'
                            , getHeadersHList :: HList ls
                            -- ^ HList of headers.
                            } deriving (Functor)

instance (NFDataHList ls, NFData a) => NFData (Headers ls a) where
    rnf (Headers x hdrs) = rnf x `seq` rnf hdrs

data ResponseHeader (sym :: Symbol) a
    = Header a
    | MissingHeader
    | UndecodableHeader ByteString
  deriving (Typeable, Eq, Show, Functor)

instance NFData a => NFData (ResponseHeader sym a) where
    rnf MissingHeader          = ()
    rnf (UndecodableHeader bs) = rnf bs
    rnf (Header x)             = rnf x

data HList a where
    HNil  :: HList '[]
    HCons :: ResponseHeader h x -> HList xs -> HList (Header h x ': xs)

class NFDataHList xs where rnfHList :: HList xs -> ()
instance NFDataHList '[] where rnfHList HNil = ()
instance (y ~ Header h x, NFData x, NFDataHList xs) => NFDataHList (y ': xs) where
    rnfHList (HCons h xs) = rnf h `seq` rnfHList xs

instance NFDataHList xs => NFData (HList xs) where
    rnf = rnfHList

type family HeaderValMap (f :: * -> *) (xs :: [*]) where
    HeaderValMap f '[]                = '[]
    HeaderValMap f (Header h x ': xs) = Header h (f x) ': HeaderValMap f xs


class BuildHeadersTo hs where
    buildHeadersTo :: [HTTP.Header] -> HList hs
    -- ^ Note: if there are multiple occurrences of a header in the argument,
    -- the values are interspersed with commas before deserialization (see
    -- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec4.html#sec4.2 RFC2616 Sec 4.2>)

instance {-# OVERLAPPING #-} BuildHeadersTo '[] where
    buildHeadersTo _ = HNil

instance {-# OVERLAPPABLE #-} ( FromHttpApiData v, BuildHeadersTo xs, KnownSymbol h )
         => BuildHeadersTo (Header h v ': xs) where
    buildHeadersTo headers =
      let wantedHeader = CI.mk . pack $ symbolVal (Proxy :: Proxy h)
          matching = snd <$> filter (\(h, _) -> h == wantedHeader) headers
      in case matching of
        [] -> MissingHeader `HCons` buildHeadersTo headers
        xs -> case parseHeader (BS.init $ BS.unlines xs) of
          Left _err -> UndecodableHeader (BS.init $ BS.unlines xs)
             `HCons` buildHeadersTo headers
          Right h   -> Header h `HCons` buildHeadersTo headers

-- * Getting headers

class GetHeaders ls where
    getHeaders :: ls -> [HTTP.Header]

-- | Auxiliary class for @'GetHeaders' ('HList' hs)@ instance
class GetHeadersFromHList hs where
    getHeadersFromHList :: HList hs  -> [HTTP.Header]

instance GetHeadersFromHList hs => GetHeaders (HList hs) where
    getHeaders = getHeadersFromHList

instance GetHeadersFromHList '[] where
    getHeadersFromHList _ = []

instance (KnownSymbol h, ToHttpApiData x, GetHeadersFromHList xs)
    => GetHeadersFromHList (Header h x ': xs)
  where
    getHeadersFromHList hdrs = case hdrs of
        Header val `HCons` rest          -> (headerName , toHeader val) : getHeadersFromHList rest
        UndecodableHeader h `HCons` rest -> (headerName,  h) : getHeadersFromHList rest
        MissingHeader `HCons` rest       -> getHeadersFromHList rest
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

instance (KnownSymbol h, GetHeadersFromHList rest, ToHttpApiData v)
    => GetHeaders' (Header h v ': rest)
  where
    getHeaders' hs = getHeadersFromHList $ getHeadersHList hs

-- * Adding headers

-- We need all these fundeps to save type inference
class AddHeader h v orig new
    | h v orig -> new, new -> h, new -> v, new -> orig where
  addOptionalHeader :: ResponseHeader h v -> orig -> new  -- ^ N.B.: The same header can't be added multiple times

-- In this instance, we add a Header on top of something that is already decorated with some headers
instance {-# OVERLAPPING #-} ( KnownSymbol h, ToHttpApiData v )
         => AddHeader h v (Headers (fst ': rest)  a) (Headers (Header h v  ': fst ': rest) a) where
    addOptionalHeader hdr (Headers resp heads) = Headers resp (HCons hdr heads)

-- In this instance, 'a' parameter is decorated with a Header.
instance {-# OVERLAPPABLE #-} ( KnownSymbol h, ToHttpApiData v , new ~ Headers '[Header h v] a)
         => AddHeader h v a new where
    addOptionalHeader hdr resp = Headers resp (HCons hdr HNil)

-- | @addHeader@ adds a header to a response. Note that it changes the type of
-- the value in the following ways:
--
--   1. A simple value is wrapped in "Headers '[hdr]":
--
-- >>> let example1 = addHeader 5 "hi" :: Headers '[Header "someheader" Int] String;
-- >>> getHeaders example1
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
addHeader :: AddHeader h v orig new => v -> orig -> new
addHeader = addOptionalHeader . Header

-- | Deliberately do not add a header to a value.
--
-- >>> let example1 = noHeader "hi" :: Headers '[Header "someheader" Int] String
-- >>> getHeaders example1
-- []
noHeader :: AddHeader h v orig new => orig -> new
noHeader = addOptionalHeader MissingHeader

class HasResponseHeader h a headers where
  hlistLookupHeader :: HList headers -> ResponseHeader h a

instance {-# OVERLAPPING #-} HasResponseHeader h a (Header h a ': rest) where
  hlistLookupHeader (HCons ha _) = ha

instance {-# OVERLAPPABLE #-} (HasResponseHeader h a rest) => HasResponseHeader h a (first ': rest) where
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
--
lookupResponseHeader :: (HasResponseHeader h a headers)
  => Headers headers r -> ResponseHeader h a
lookupResponseHeader = hlistLookupHeader . getHeadersHList

-- $setup
-- >>> :set -XFlexibleContexts
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
