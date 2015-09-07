{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | A @CSV@ empty datatype with `MimeRender` and `MimeUnrender` instances for
-- @cassava@'s encoding and decoding classes.
--
-- >>> type Eg = Get '[(CSV', MyEncodeOptions)] [(Int, String)]
--
-- Default encoding and decoding options are also provided, along with the
-- @CSV@ type synonym that uses them.
--
-- >>> type EgDefault = Get '[CSV] [(Int, String)]
module Servant.CSV.Cassava where

import           Data.Csv
import           Data.Proxy         (Proxy (..))
import           Data.Typeable      (Typeable)
import           Data.Vector        (Vector)
import           GHC.Generics       (Generic)
import qualified Network.HTTP.Media as M
import           Servant.API        (Accept (..), MimeRender (..),
                                     MimeUnrender (..))

data CSV' deriving (Typeable, Generic)

type CSV = (CSV', DefaultDecodeOpts)

-- | @text/csv;charset=utf-8@
instance Accept (CSV', a) where
    contentType _ = "text" M.// "csv" M./: ("charset", "utf-8")

-- * Encoding

-- ** Instances

-- | Encode with 'encodeByNameWith'. The 'Header' param is used for determining
-- the order of headers and fields.
instance ( ToNamedRecord a, EncodeOpts opt
         ) => MimeRender (CSV', opt) (Header, [a]) where
    mimeRender _ (hdr, vals) = encodeByNameWith (encodeOpts p) hdr vals
      where p = Proxy :: Proxy opt

-- | Encode with 'encodeDefaultOrderedByNameWith'
instance ( DefaultOrdered a, ToNamedRecord a, EncodeOpts opt
         ) => MimeRender (CSV', opt) [a] where
    mimeRender _ = encodeDefaultOrderedByNameWith (encodeOpts p)
      where p = Proxy :: Proxy opt


-- ** Encode Options

class EncodeOpts a where
    encodeOpts :: Proxy a -> EncodeOptions

data DefaultEncodeOpts deriving (Typeable, Generic)

instance EncodeOpts DefaultEncodeOpts where
    encodeOpts _ = defaultEncodeOptions

-- * Decoding

-- ** Instances

-- | Decode with 'decodeByNameWith'
instance ( FromNamedRecord a, DecodeOpts opt
         ) => MimeUnrender (CSV', opt) (Header, Vector a) where
    mimeUnrender _ = decodeByNameWith (decodeOpts p)
      where p = Proxy :: Proxy opt

-- | Decode with 'decodeWith'. Assumes data has headers, which are stripped.
instance ( FromRecord a, DecodeOpts opt
         ) => MimeUnrender (CSV', opt) (Vector a) where
    mimeUnrender _ = decodeWith (decodeOpts p) HasHeader
      where p = Proxy :: Proxy opt

-- ** Decode Options

class DecodeOpts a where
    decodeOpts :: Proxy a -> DecodeOptions

data DefaultDecodeOpts deriving (Typeable, Generic)

instance DecodeOpts DefaultDecodeOpts where
    decodeOpts _ = defaultDecodeOptions
