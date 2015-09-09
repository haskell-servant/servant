{-# LANGUAGE CPP                   #-}
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
{-# OPTIONS_HADDOCK not-home       #-}

-- | A collection of basic Content-Types (also known as Internet Media
-- Types, or MIME types). Additionally, this module provides classes that
-- encapsulate how to serialize or deserialize values to or from
-- a particular Content-Type.
--
-- Content-Types are used in `ReqBody` and the method combinators:
--
-- >>> type MyEndpoint = ReqBody '[JSON, PlainText] Book :> Get '[JSON, PlainText] :> Book
--
-- Meaning the endpoint accepts requests of Content-Type @application/json@
-- or @text/plain;charset-utf8@, and returns data in either one of those
-- formats (depending on the @Accept@ header).
--
-- If you would like to support Content-Types beyond those provided here,
-- then:
--
--      (1) Declare a new data type with no constructors (e.g. @data HTML@).
--      (2) Make an instance of it for `Accept`.
--      (3) If you want to be able to serialize data *into* that
--      Content-Type, make an instance of it for `MimeRender`.
--      (4) If you want to be able to deserialize data *from* that
--      Content-Type, make an instance of it for `MimeUnrender`.
--
-- Note that roles are reversed in @servant-server@ and @servant-client@:
-- to be able to serve (or even typecheck) a @Get '[JSON, XML] MyData@,
-- you'll need to have the appropriate `MimeRender` instances in scope,
-- whereas to query that endpoint with @servant-client@, you'll need
-- a `MimeUnrender` instance in scope.
module Servant.API.ContentTypes
    (
    -- * Provided Content-Types
      JSON
    , PlainText
    , FormUrlEncoded
    , OctetStream

    -- * Building your own Content-Type
    , Accept(..)
    , MimeRender(..)
    , MimeUnrender(..)

    -- * Internal
    , AcceptHeader(..)
    , AllCTRender(..)
    , AllCTUnrender(..)
    , AllMimeRender(..)
    , AllMimeUnrender(..)
    , FromFormUrlEncoded(..)
    , ToFormUrlEncoded(..)
    , IsNonEmpty
    , eitherDecodeLenient
    ) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative              ((*>), (<*))
#endif
import           Control.Arrow                    (left)
import           Control.Monad
import           Data.Aeson                       (FromJSON, ToJSON, encode,
                                                   parseJSON)
import           Data.Aeson.Parser                (value)
import           Data.Aeson.Types                 (parseEither)
import           Data.Attoparsec.ByteString.Char8 (endOfInput, parseOnly,
                                                   skipSpace, (<?>))
import qualified Data.ByteString                  as BS
import           Data.ByteString.Lazy             (ByteString, fromStrict,
                                                   toStrict)
import qualified Data.ByteString.Lazy             as B
import qualified Data.ByteString.Lazy.Char8       as BC
import           Data.Monoid
import           Data.String.Conversions          (cs)
import qualified Data.Text                        as TextS
import qualified Data.Text.Encoding               as TextS
import qualified Data.Text.Lazy                   as TextL
import qualified Data.Text.Lazy.Encoding          as TextL
import           Data.Typeable
import           GHC.Exts                         (Constraint)
import qualified Network.HTTP.Media               as M
import           Network.URI                      (escapeURIString,
                                                   isUnreserved, unEscapeString)

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
-- >>> import Network.HTTP.Media ((//), (/:))
-- >>> data HTML
-- >>> :{
--instance Accept HTML where
--    contentType _ = "text" // "html" /: ("charset", "utf-8")
-- :}
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
-- >    mimeRender _ val = pack ("This is MINE! " ++ show val)
-- >
-- > type MyAPI = "path" :> Get '[MyContentType] Int
--
class Accept ctype => MimeRender ctype a where
    mimeRender  :: Proxy ctype -> a -> ByteString

class AllCTRender (list :: [*]) a where
    -- If the Accept header can be matched, returns (Just) a tuple of the
    -- Content-Type and response (serialization of @a@ into the appropriate
    -- mimetype).
    handleAcceptH :: Proxy list -> AcceptHeader -> a -> Maybe (ByteString, ByteString)

instance ( AllMimeRender ctyps a, IsNonEmpty ctyps
         ) => AllCTRender ctyps a where
    handleAcceptH _ (AcceptHeader accept) val = M.mapAcceptMedia lkup accept
      where pctyps = Proxy :: Proxy ctyps
            amrs = allMimeRender pctyps val
            lkup = fmap (\(a,b) -> (a, (fromStrict $ M.renderHeader a, b))) amrs


--------------------------------------------------------------------------
-- * Unrender

-- | Instantiate this class to register a way of deserializing a type based
-- on the request's @Content-Type@ header.
--
-- >>> import Network.HTTP.Media hiding (Accept)
-- >>> import qualified Data.ByteString.Lazy.Char8 as BSC
-- >>> data MyContentType = MyContentType String
--
-- >>> :{
--instance Accept MyContentType where
--    contentType _ = "example" // "prs.me.mine" /: ("charset", "utf-8")
-- :}
--
-- >>> :{
--instance Read a => MimeUnrender MyContentType a where
--    mimeUnrender _ bs = case BSC.take 12 bs of
--      "MyContentType" -> return . read . BSC.unpack $ BSC.drop 12 bs
--      _ -> Left "didn't start with the magic incantation"
-- :}
--
-- >>> type MyAPI = "path" :> ReqBody '[MyContentType] Int :> Get '[JSON] Int
--
class Accept ctype => MimeUnrender ctype a where
    mimeUnrender :: Proxy ctype -> ByteString -> Either String a

class (IsNonEmpty list) => AllCTUnrender (list :: [*]) a where
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
class AllMimeRender (list :: [*]) a where
    allMimeRender :: Proxy list
                  -> a                              -- value to serialize
                  -> [(M.MediaType, ByteString)]    -- content-types/response pairs

instance ( MimeRender ctyp a ) => AllMimeRender '[ctyp] a where
    allMimeRender _ a = [(contentType pctyp, mimeRender pctyp a)]
        where pctyp = Proxy :: Proxy ctyp

instance ( MimeRender ctyp a
         , AllMimeRender (ctyp' ': ctyps) a
         ) => AllMimeRender (ctyp ': ctyp' ': ctyps) a where
    allMimeRender _ a = (contentType pctyp, mimeRender pctyp a)
                       :(allMimeRender pctyps a)
        where pctyp = Proxy :: Proxy ctyp
              pctyps = Proxy :: Proxy (ctyp' ': ctyps)


instance AllMimeRender '[] a where
    allMimeRender _ _ = []

--------------------------------------------------------------------------
-- Check that all elements of list are instances of MimeUnrender
--------------------------------------------------------------------------
class AllMimeUnrender (list :: [*]) a where
    allMimeUnrender :: Proxy list
                    -> ByteString
                    -> [(M.MediaType, Either String a)]

instance AllMimeUnrender '[] a where
    allMimeUnrender _ _ = []

instance ( MimeUnrender ctyp a
         , AllMimeUnrender ctyps a
         ) => AllMimeUnrender (ctyp ': ctyps) a where
    allMimeUnrender _ val = (contentType pctyp, mimeUnrender pctyp val)
                           :(allMimeUnrender pctyps val)
        where pctyp = Proxy :: Proxy ctyp
              pctyps = Proxy :: Proxy ctyps

type family IsNonEmpty (list :: [*]) :: Constraint where
    IsNonEmpty (x ': xs)   = ()


--------------------------------------------------------------------------
-- * MimeRender Instances

-- | `encode`
instance ToJSON a => MimeRender JSON a where
    mimeRender _ = encode

-- | @encodeFormUrlEncoded . toFormUrlEncoded@
-- Note that the @mimeUnrender p (mimeRender p x) == Right x@ law only
-- holds if every element of x is non-null (i.e., not @("", "")@)
instance ToFormUrlEncoded a => MimeRender FormUrlEncoded a where
    mimeRender _ = encodeFormUrlEncoded . toFormUrlEncoded

-- | `TextL.encodeUtf8`
instance MimeRender PlainText TextL.Text where
    mimeRender _ = TextL.encodeUtf8

-- | @fromStrict . TextS.encodeUtf8@
instance MimeRender PlainText TextS.Text where
    mimeRender _ = fromStrict . TextS.encodeUtf8

-- | @BC.pack@
instance MimeRender PlainText String where
    mimeRender _ = BC.pack

-- | @id@
instance MimeRender OctetStream ByteString where
    mimeRender _ = id

-- | `fromStrict`
instance MimeRender OctetStream BS.ByteString where
    mimeRender _ = fromStrict


--------------------------------------------------------------------------
-- * MimeUnrender Instances

-- | Like 'Data.Aeson.eitherDecode' but allows all JSON values instead of just
-- objects and arrays.
--
-- Will handle trailing whitespace, but not trailing junk. ie.
--
-- >>> eitherDecodeLenient "1 " :: Either String Int
-- Right 1
--
-- >>> eitherDecodeLenient "1 junk" :: Either String Int
-- Left "trailing junk after valid JSON: endOfInput"
eitherDecodeLenient :: FromJSON a => ByteString -> Either String a
eitherDecodeLenient input =
    parseOnly parser (cs input) >>= parseEither parseJSON
  where
    parser = skipSpace
          *> Data.Aeson.Parser.value
          <* skipSpace
          <* (endOfInput <?> "trailing junk after valid JSON")

-- | `eitherDecode`
instance FromJSON a => MimeUnrender JSON a where
    mimeUnrender _ = eitherDecodeLenient

-- | @decodeFormUrlEncoded >=> fromFormUrlEncoded@
-- Note that the @mimeUnrender p (mimeRender p x) == Right x@ law only
-- holds if every element of x is non-null (i.e., not @("", "")@)
instance FromFormUrlEncoded a => MimeUnrender FormUrlEncoded a where
    mimeUnrender _ = decodeFormUrlEncoded >=> fromFormUrlEncoded

-- | @left show . TextL.decodeUtf8'@
instance MimeUnrender PlainText TextL.Text where
    mimeUnrender _ = left show . TextL.decodeUtf8'

-- | @left show . TextS.decodeUtf8' . toStrict@
instance MimeUnrender PlainText TextS.Text where
    mimeUnrender _ = left show . TextS.decodeUtf8' . toStrict

-- | @Right . BC.unpack@
instance MimeUnrender PlainText String where
    mimeUnrender _ = Right . BC.unpack

-- | @Right . id@
instance MimeUnrender OctetStream ByteString where
    mimeUnrender _ = Right . id

-- | @Right . toStrict@
instance MimeUnrender OctetStream BS.ByteString where
    mimeUnrender _ = Right . toStrict


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
        encodePair (k, "") = escape k
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
                [k] -> return ( unescape k, "" )
                _ -> Left $ "not a valid pair: " <> cs p
        unescape :: TextS.Text -> TextS.Text
        unescape = cs . unEscapeString . cs . TextS.intercalate "%20" . TextS.splitOn "+"
    mapM parsePair xs

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
