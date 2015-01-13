{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.Server.ContentTypes where

import Control.Monad (join)
import Data.Aeson (ToJSON(..), FromJSON(..), encode, decode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as BS
import Data.Proxy (Proxy(..))
import Data.String.Conversions (cs)
import qualified Data.Text.Lazy.Encoding as Text
import qualified Data.Text.Lazy as Text
import GHC.Exts (Constraint)
import qualified Network.HTTP.Media as M


import Servant.API ( XML, HTML, JSON, JavaScript, CSS, PlainText
                   , OctetStream)

-- * Accept class

-- | Instances of 'Accept' represent mimetypes. They are used for matching
-- against the @Accept@ HTTP header of the request, and for setting the
-- @Content-Type@ header of the response
--
-- Example:
--
-- > instance Accept HTML where
-- >    contentType _ = "text" // "html"
--
class Accept ctype where
    contentType   :: Proxy ctype -> M.MediaType

-- | @text/html;charset=utf-8@
instance Accept HTML where
    contentType _ = "text" M.// "html" M./: ("charset", "utf-8")

-- | @application/json;charset=utf-8@
instance Accept JSON where
    contentType _ = "application" M.// "json" M./: ("charset", "utf-8")

-- | @application/xml;charset=utf-8@
instance Accept XML where
    contentType _ = "application" M.// "xml" M./: ("charset", "utf-8")

-- | @application/javascript;charset=utf-8@
instance Accept JavaScript where
    contentType _ = "application" M.// "javascript" M./: ("charset", "utf-8")

-- | @text/css;charset=utf-8@
instance Accept CSS where
    contentType _ = "text" M.// "css" M./: ("charset", "utf-8")

-- | @text/plain;charset=utf-8@
instance Accept PlainText where
    contentType _ = "text" M.// "plain" M./: ("charset", "utf-8")

-- | @application/octet-stream@
instance Accept OctetStream where
    contentType _ = "application" M.// "octet-stream"

newtype AcceptHeader = AcceptHeader BS.ByteString
    deriving (Eq, Show)

-- * Render (serializing)

-- | Instantiate this class to register a way of serializing a type based
-- on the @Accept@ header.
--
-- Example:
--
-- > data MyContentType
-- >
-- > instance Accept MyContentType where
-- >    contentType _ = "example" // "prs.me.mine" /: ("charset", "utf-8")
-- >
-- > instance Show a => MimeRender MyContentType where
-- >    toByteString _ val = pack ("This is MINE! " ++ show val)
-- >
-- > type MyAPI = "path" :> Get '[MyContentType] Int
class Accept ctype => MimeRender ctype a where
    toByteString  :: Proxy ctype -> a -> ByteString

class AllCTRender list a where
    -- If the Accept header can be matched, returns (Just) a tuple of the
    -- Content-Type and response (serialization of @a@ into the appropriate
    -- mimetype).
    handleAcceptH :: Proxy list -> AcceptHeader -> a -> Maybe (ByteString, ByteString)

instance ( AllMimeRender ctyps a, IsNonEmpty ctyps
         ) => AllCTRender ctyps a where
    handleAcceptH _ (AcceptHeader accept) val = M.mapAcceptMedia lkup accept
      where pctyps = Proxy :: Proxy ctyps
            amrs = amr pctyps val
            lkup = zip (map fst amrs) $ map (\(a,b) -> (cs $ show a, b)) amrs




--------------------------------------------------------------------------
-- * MimeRender Instances

-- | @encode@
instance ToJSON a => MimeRender JSON a where
    toByteString _ = encode

-- | @encodeUtf8@
instance MimeRender PlainText Text.Text where
    toByteString _ = Text.encodeUtf8

--------------------------------------------------------------------------
-- * Unrender
class Accept ctype => MimeUnrender ctype a where
    fromByteString :: Proxy ctype -> ByteString -> Maybe a

class AllCTUnrender list a where
    handleCTypeH :: Proxy list
                 -> ByteString     -- Content-Type header
                 -> ByteString     -- Request body
                 -> Maybe a

instance ( AllMimeUnrender ctyps a, IsNonEmpty ctyps
         ) => AllCTUnrender ctyps a where
    handleCTypeH _ ctypeH body = join $ M.mapContentMedia lkup (cs ctypeH)
      where lkup = amu (Proxy :: Proxy ctyps) body

--------------------------------------------------------------------------
-- * Utils (Internal)


--------------------------------------------------------------------------
-- Check that all elements of list are instances of MimeRender
--------------------------------------------------------------------------
class AllMimeRender ls a where
    amr :: Proxy ls
        -> a                              -- value to serialize
        -> [(M.MediaType, ByteString)]    -- content-types/response pairs

instance ( MimeRender ctyp a ) => AllMimeRender '[ctyp] a where
    amr _ a = [(contentType pctyp, toByteString pctyp a)]
        where pctyp = Proxy :: Proxy ctyp

instance ( MimeRender ctyp a
         , MimeRender ctyp' a        -- at least two elems to avoid overlap
         , AllMimeRender ctyps a
         ) => AllMimeRender (ctyp ': ctyp' ': ctyps) a where
    amr _ a = (contentType pctyp, toByteString pctyp a)
             :(contentType pctyp', toByteString pctyp' a)
             :(amr pctyps a)
        where pctyp = Proxy :: Proxy ctyp
              pctyps = Proxy :: Proxy ctyps
              pctyp' = Proxy :: Proxy ctyp'


instance AllMimeRender '[] a where
    amr _ _ = []

--------------------------------------------------------------------------
-- Check that all elements of list are instances of MimeUnrender
--------------------------------------------------------------------------
class AllMimeUnrender ls a where
    amu :: Proxy ls -> ByteString -> [(M.MediaType, Maybe a)]

instance ( MimeUnrender ctyp a ) => AllMimeUnrender '[ctyp] a where
    amu _ val = [(contentType pctyp, fromByteString pctyp val)]
        where pctyp = Proxy :: Proxy ctyp

instance ( MimeUnrender ctyp a
         , MimeUnrender ctyp' a
         , AllMimeUnrender ctyps a
         ) => AllMimeUnrender (ctyp ': ctyp' ': ctyps) a where
    amu _ val = (contentType pctyp, fromByteString pctyp val)
               :(contentType pctyp', fromByteString pctyp' val)
               :(amu pctyps val)
        where pctyp = Proxy :: Proxy ctyp
              pctyps = Proxy :: Proxy ctyps
              pctyp' = Proxy :: Proxy ctyp'

type family IsNonEmpty (ls::[*]) :: Constraint where
    IsNonEmpty '[] = 'False ~ 'True
    IsNonEmpty x   = ()

--------------------------------------------------------------------------
-- * MimeUnrender Instances

-- | @decode@
instance FromJSON a => MimeUnrender JSON a where
    fromByteString _ = decode

-- | @Text.decodeUtf8'@
instance MimeUnrender PlainText Text.Text where
    fromByteString _ = either (const Nothing) Just . Text.decodeUtf8'

