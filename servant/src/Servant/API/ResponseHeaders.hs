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
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
#if !MIN_VERSION_base(4,8,0)
{-# LANGUAGE OverlappingInstances   #-}
#endif

-- | This module provides facilities for adding headers to a response.
--
-- >>> let headerVal = addHeader "some-url" 5 :: Headers '[Header "Location" String] Int
--
-- The value is added to the header specified by the type (@Location@ in the
-- example above).
module Servant.API.ResponseHeaders
    ( Headers
    , getResponse
    , getHeaders
    , AddHeader(addHeader)
    ) where

import           Data.ByteString.Char8       (pack)
import           Data.ByteString.Conversion  (ToByteString, toByteString')
import qualified Data.CaseInsensitive        as CI
import           Data.Proxy
import           GHC.TypeLits                (KnownSymbol, symbolVal)
import qualified Network.HTTP.Types.Header   as HTTP

import           Servant.API.Header          (Header (..))

-- | Response Header objects. You should never need to construct one directly.
-- Instead, use 'addHeader'.
data Headers ls a = Headers { getResponse :: a
                            -- ^ The underlying value of a 'Headers'
                            , getHeaders :: [HTTP.Header]
                            -- ^ The list of header values of a 'Headers'.
                            -- These are guaranteed to correspond with the
                            -- first type of @Headers@ if constructed with
                            -- 'addHeader'.
                            } deriving (Eq, Show, Functor)

-- We need all these fundeps to save type inference
class AddHeader h v orig new
    | h v orig -> new, new -> h, new -> v, new -> orig where
  addHeader :: v -> orig -> new

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPING #-}
#endif
         ( KnownSymbol h, ToByteString v
         ) => AddHeader h v (Headers (fst ': rest)  a) (Headers (Header h v ': fst ': rest) a) where
    addHeader a (Headers resp heads) = Headers resp ((headerName, toByteString' a) : heads)
      where
        headerName = CI.mk . pack $ symbolVal (Proxy :: Proxy h)

instance
#if MIN_VERSION_base(4,8,0)
         {-# OVERLAPPABLE #-}
#endif
         ( KnownSymbol h, ToByteString v
         , new ~ (Headers '[Header h v] a)
         ) => AddHeader h v a new where
    addHeader a resp = Headers resp [(headerName, toByteString' a)]
      where
        headerName = CI.mk . pack $ symbolVal (Proxy :: Proxy h)


-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
