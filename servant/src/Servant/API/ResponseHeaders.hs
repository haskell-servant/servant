{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_HADDOCK not-home        #-}

#include "overlapping-compat.h"
-- | This module provides facilities for adding headers to a response.
--
-- >>> let headerVal = addHeader "some-url" 5 :: Headers '[Header "Location" String] Int
--
-- The value is added to the header specified by the type (@Location@ in the
-- example above).
module Servant.API.ResponseHeaders
    ( Headers(..)
    , AddHeader(addHeader)
    , BuildHeadersTo(buildHeadersTo)
    , GetHeaders(getHeaders)
    , HeaderValMap
    , HList(..)
    ) where

import           Data.ByteString.Char8       as BS (pack, unlines, init)
import           Data.ByteString.Conversion  (ToByteString, toByteString',
                                              FromByteString, fromByteString)
import qualified Data.CaseInsensitive        as CI
import           Data.Proxy
import           GHC.TypeLits                (KnownSymbol, symbolVal)
import qualified Network.HTTP.Types.Header   as HTTP

import           Servant.API.Header          (Header (..))
import           Prelude                     ()
import           Prelude.Compat

-- | Response Header objects. You should never need to construct one directly.
-- Instead, use 'addHeader'.
data Headers ls a = Headers { getResponse :: a
                            -- ^ The underlying value of a 'Headers'
                            , getHeadersHList :: HList ls
                            -- ^ HList of headers.
                            } deriving (Functor)

data HList a where
    HNil  :: HList '[]
    HCons :: Header h x -> HList xs -> HList (Header h x ': xs)

type family HeaderValMap (f :: * -> *) (xs :: [*]) where
    HeaderValMap f '[]                = '[]
    HeaderValMap f (Header h x ': xs) = Header h (f x) ': (HeaderValMap f xs)


class BuildHeadersTo hs where
    buildHeadersTo :: [HTTP.Header] -> HList hs
    -- ^ Note: if there are multiple occurences of a header in the argument,
    -- the values are interspersed with commas before deserialization (see
    -- <http://www.w3.org/Protocols/rfc2616/rfc2616-sec4.html#sec4.2 RFC2616 Sec 4.2>)

instance OVERLAPPING_ BuildHeadersTo '[] where
    buildHeadersTo _ = HNil

instance OVERLAPPABLE_ ( FromByteString v, BuildHeadersTo xs, KnownSymbol h
                       , Contains h xs ~ 'False)
         => BuildHeadersTo ((Header h v) ': xs) where
    buildHeadersTo headers =
      let wantedHeader = CI.mk . pack $ symbolVal (Proxy :: Proxy h)
          matching = snd <$> filter (\(h, _) -> h == wantedHeader) headers
      in case matching of
        [] -> MissingHeader `HCons` buildHeadersTo headers
        xs -> case fromByteString (BS.init $ BS.unlines xs) of
          Nothing -> UndecodableHeader (BS.init $ BS.unlines xs)
             `HCons` buildHeadersTo headers
          Just h   -> Header h `HCons` buildHeadersTo headers

-- * Getting

class GetHeaders ls where
    getHeaders :: ls -> [HTTP.Header]

instance OVERLAPPING_ GetHeaders (HList '[]) where
    getHeaders _ = []

instance OVERLAPPABLE_ ( KnownSymbol h, ToByteString x, GetHeaders (HList xs))
         => GetHeaders (HList (Header h x ': xs)) where
    getHeaders hdrs = case hdrs of
        Header val `HCons` rest -> (headerName , toByteString' val):getHeaders rest
        UndecodableHeader h `HCons` rest -> (headerName,  h) : getHeaders rest
        MissingHeader `HCons` rest -> getHeaders rest
      where headerName = CI.mk . pack $ symbolVal (Proxy :: Proxy h)

instance OVERLAPPING_ GetHeaders (Headers '[] a) where
    getHeaders _ = []

instance OVERLAPPABLE_ ( KnownSymbol h, GetHeaders (HList rest), ToByteString v)
         => GetHeaders (Headers (Header h v ': rest) a) where
    getHeaders hs = getHeaders $ getHeadersHList hs

-- * Adding

-- We need all these fundeps to save type inference
class AddHeader h v orig new
    | h v orig -> new, new -> h, new -> v, new -> orig where
  addHeader :: v -> orig -> new  -- ^ N.B.: The same header can't be added multiple times


instance OVERLAPPING_ ( KnownSymbol h, ToByteString v, Contains h (fst ': rest) ~ 'False)
         => AddHeader h v (Headers (fst ': rest)  a) (Headers (Header h v  ': fst ': rest) a) where
    addHeader a (Headers resp heads) = Headers resp (HCons (Header a) heads)

instance OVERLAPPABLE_ ( KnownSymbol h, ToByteString v
                       , new ~ (Headers '[Header h v] a))
         => AddHeader h v a new where
    addHeader a resp = Headers resp (HCons (Header a) HNil)

type family Contains x xs where
    Contains x ((Header x a) ': xs) = 'True
    Contains x ((Header y a) ': xs) = Contains x xs
    Contains x '[]                  = 'False

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
