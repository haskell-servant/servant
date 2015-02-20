{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Servant.API.ContentTypes where

import           Control.Arrow           (left)
import           Control.Monad
import           Data.Aeson              (FromJSON, ToJSON, eitherDecode,
                                          encode)
import qualified Data.ByteString         as BS
import           Data.ByteString.Lazy    (ByteString, fromStrict, toStrict)
import qualified Data.ByteString.Lazy    as B
import           Data.String.Conversions (cs)
import           Data.Monoid
import qualified Data.Text.Lazy          as TextL
import qualified Data.Text.Lazy.Encoding as TextL
import qualified Data.Text               as TextS
import qualified Data.Text.Encoding      as TextS
import           Data.Typeable
import           GHC.Exts                (Constraint)
import qualified Network.HTTP.Media      as M
import           Network.URI             (unEscapeString, escapeURIString,
                                          isUnreserved)

-- * Provided content types
data JSON deriving Typeable
data PlainText deriving Typeable
data FormUrlEncoded deriving Typeable
data OctetStream deriving Typeable

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

-- | @application/json@
instance Accept JSON where
    contentType _ = "application" M.// "json"

-- | @application/x-www-form-urlencoded@
instance Accept FormUrlEncoded where
    contentType _ = "application" M.// "x-www-form-urlencoded"

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
--
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
            amrs = allMimeRender pctyps val
            lkup = fmap (\(a,b) -> (a, (cs $ show a, b))) amrs


--------------------------------------------------------------------------
-- * Unrender

-- | Instantiate this class to register a way of deserializing a type based
-- on the request's @Content-Type@ header.
--
-- > data MyContentType = MyContentType String
-- >
-- > instance Accept MyContentType where
-- >    contentType _ = "example" // "prs.me.mine" /: ("charset", "utf-8")
-- >
-- > instance Show a => MimeUnrender MyContentType where
-- >    fromByteString _ bs = MyContentType $ unpack bs
-- >
-- > type MyAPI = "path" :> ReqBody '[MyContentType] :> Get '[JSON] Int
--
class Accept ctype => MimeUnrender ctype a where
    fromByteString :: Proxy ctype -> ByteString -> Either String a

class (IsNonEmpty list) => AllCTUnrender list a where
    handleCTypeH :: Proxy list
                 -> ByteString     -- Content-Type header
                 -> ByteString     -- Request body
                 -> Maybe (Either String a)

instance ( AllMimeUnrender ctyps a, IsNonEmpty ctyps
         ) => AllCTUnrender ctyps a where
    handleCTypeH _ ctypeH body = M.mapContentMedia lkup (cs ctypeH)
      where lkup = allMimeUnrender (Proxy :: Proxy ctyps) body

--------------------------------------------------------------------------
-- * Utils (Internal)


--------------------------------------------------------------------------
-- Check that all elements of list are instances of MimeRender
--------------------------------------------------------------------------
class AllMimeRender ls a where
    allMimeRender :: Proxy ls
                  -> a                              -- value to serialize
                  -> [(M.MediaType, ByteString)]    -- content-types/response pairs

instance ( MimeRender ctyp a ) => AllMimeRender '[ctyp] a where
    allMimeRender _ a = [(contentType pctyp, toByteString pctyp a)]
        where pctyp = Proxy :: Proxy ctyp

instance ( MimeRender ctyp a
         , AllMimeRender (ctyp' ': ctyps) a
         ) => AllMimeRender (ctyp ': ctyp' ': ctyps) a where
    allMimeRender _ a = (contentType pctyp, toByteString pctyp a)
                       :(allMimeRender pctyps a)
        where pctyp = Proxy :: Proxy ctyp
              pctyps = Proxy :: Proxy (ctyp' ': ctyps)


instance AllMimeRender '[] a where
    allMimeRender _ _ = []

--------------------------------------------------------------------------
-- Check that all elements of list are instances of MimeUnrender
--------------------------------------------------------------------------
class AllMimeUnrender ls a where
    allMimeUnrender :: Proxy ls -> ByteString -> [(M.MediaType, Either String a)]

instance AllMimeUnrender '[] a where
    allMimeUnrender _ _ = []

instance ( MimeUnrender ctyp a
         , AllMimeUnrender ctyps a
         ) => AllMimeUnrender (ctyp ': ctyps) a where
    allMimeUnrender _ val = (contentType pctyp, fromByteString pctyp val)
                           :(allMimeUnrender pctyps val)
        where pctyp = Proxy :: Proxy ctyp
              pctyps = Proxy :: Proxy ctyps

type family IsNonEmpty (ls::[*]) :: Constraint where
    IsNonEmpty (x ': xs)   = ()


--------------------------------------------------------------------------
-- * MimeRender Instances

-- | `encode`
instance ToJSON a => MimeRender JSON a where
    toByteString _ = encode

-- | `encodeFormUrlEncoded . toFormUrlEncoded`
instance ToFormUrlEncoded a => MimeRender FormUrlEncoded a where
    toByteString _ = encodeFormUrlEncoded . toFormUrlEncoded

-- | `TextL.encodeUtf8`
instance MimeRender PlainText TextL.Text where
    toByteString _ = TextL.encodeUtf8

-- | `fromStrict . TextS.encodeUtf8`
instance MimeRender PlainText TextS.Text where
    toByteString _ = fromStrict . TextS.encodeUtf8

-- | `id`
instance MimeRender OctetStream ByteString where
    toByteString _ = id

-- | `fromStrict`
instance MimeRender OctetStream BS.ByteString where
    toByteString _ = fromStrict


--------------------------------------------------------------------------
-- * MimeUnrender Instances

-- | `eitherDecode`
instance FromJSON a => MimeUnrender JSON a where
    fromByteString _ = eitherDecode

-- | `decodeFormUrlEncoded >=> fromFormUrlEncoded`
instance FromFormUrlEncoded a => MimeUnrender FormUrlEncoded a where
    fromByteString _ = decodeFormUrlEncoded >=> fromFormUrlEncoded

-- | `left show . TextL.decodeUtf8'`
instance MimeUnrender PlainText TextL.Text where
    fromByteString _ = left show . TextL.decodeUtf8'

-- | `left show . TextS.decodeUtf8' . toStrict`
instance MimeUnrender PlainText TextS.Text where
    fromByteString _ = left show . TextS.decodeUtf8' . toStrict

-- | `Right . id`
instance MimeUnrender OctetStream ByteString where
    fromByteString _ = Right . id

-- | `Right . toStrict`
instance MimeUnrender OctetStream BS.ByteString where
    fromByteString _ = Right . toStrict


--------------------------------------------------------------------------
-- * FormUrlEncoded

-- | A type that can be converted to @application/x-www-form-urlencoded@
class ToFormUrlEncoded a where
  toFormUrlEncoded :: a -> [(TextS.Text, TextS.Text)]

instance ToFormUrlEncoded [(TextS.Text, TextS.Text)] where
  toFormUrlEncoded = id

-- | A type that can be converted from @application/x-www-form-urlencoded@,
-- with the possibility of failure.
class FromFormUrlEncoded a where
  fromFormUrlEncoded :: [(TextS.Text, TextS.Text)] -> Either String a

instance FromFormUrlEncoded [(TextS.Text, TextS.Text)] where
  fromFormUrlEncoded = return

encodeFormUrlEncoded :: [(TextS.Text, TextS.Text)] -> ByteString
encodeFormUrlEncoded xs =
    let escape :: TextS.Text -> ByteString
        escape = cs . escapeURIString isUnreserved . cs
        encodePair :: (TextS.Text, TextS.Text) -> ByteString
        encodePair (k, v) = escape k <> "=" <> escape v
    in B.intercalate "&" $ map encodePair xs

decodeFormUrlEncoded :: ByteString -> Either String [(TextS.Text, TextS.Text)]
decodeFormUrlEncoded "" = return []
decodeFormUrlEncoded q = do
    let xs :: [TextS.Text]
        xs = TextS.splitOn "&" . cs $ q
        parsePair :: TextS.Text -> Either String (TextS.Text, TextS.Text)
        parsePair p =
            case TextS.splitOn "=" p of
                [k,v] -> return ( unescape k
                                , unescape v
                                )
                _ -> Left $ "not a valid pair: " <> cs p
        unescape :: TextS.Text -> TextS.Text
        unescape = cs . unEscapeString . cs . TextS.intercalate "%20" . TextS.splitOn "+"
    mapM parsePair xs
