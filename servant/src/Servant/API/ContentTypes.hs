{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_HADDOCK not-home       #-}

#include "overlapping-compat.h"

-- | A collection of basic Content-Types (also known as Internet Media
-- Types, or MIME types). Additionally, this module provides classes that
-- encapsulate how to serialize or deserialize values to or from
-- a particular Content-Type.
--
-- Content-Types are used in `ReqBody` and the method combinators:
--
-- >>> type MyEndpoint = ReqBody '[JSON, PlainText] Book :> Get '[JSON, PlainText] Book
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

    -- * NoContent
    , NoContent(..)

    -- * Internal
    , AcceptHeader(..)
    , AllCTRender(..)
    , AllCTUnrender(..)
    , AllMime(..)
    , AllMimeRender(..)
    , AllMimeUnrender(..)
    , eitherDecodeLenient
    , canHandleAcceptH
    ) where

import           Control.Arrow                    (left)
import           Control.Monad.Compat
import           Data.Aeson                       (FromJSON(..), ToJSON(..), encode)
import           Data.Aeson.Parser                (value)
import           Data.Aeson.Types                 (parseEither)
import           Data.Attoparsec.ByteString.Char8 (endOfInput, parseOnly,
                                                   skipSpace, (<?>))
import qualified Data.ByteString                  as BS
import           Data.ByteString.Lazy             (ByteString, fromStrict,
                                                   toStrict)
import qualified Data.ByteString.Lazy.Char8       as BC
import qualified Data.List.NonEmpty               as NE
import           Data.Maybe                       (isJust)
import           Data.String.Conversions          (cs)
import qualified Data.Text                        as TextS
import qualified Data.Text.Encoding               as TextS
import qualified Data.Text.Lazy                   as TextL
import qualified Data.Text.Lazy.Encoding          as TextL
import           Data.Typeable
import           GHC.Generics                     (Generic)
import qualified Network.HTTP.Media               as M
import           Web.FormUrlEncoded               (FromForm, ToForm,
                                                   urlEncodeAsForm,
                                                   urlDecodeAsForm)
import           Prelude                          ()
import           Prelude.Compat

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
    contentType = NE.head . contentTypes

    contentTypes  :: Proxy ctype -> NE.NonEmpty M.MediaType
    contentTypes  =  (NE.:| []) . contentType

    {-# MINIMAL contentType | contentTypes #-}

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
    deriving (Eq, Show, Read, Typeable, Generic)

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
-- > instance Show a => MimeRender MyContentType a where
-- >    mimeRender _ val = pack ("This is MINE! " ++ show val)
-- >
-- > type MyAPI = "path" :> Get '[MyContentType] Int
--
class Accept ctype => MimeRender ctype a where
    mimeRender  :: Proxy ctype -> a -> ByteString

class (AllMime list) => AllCTRender (list :: [*]) a where
    -- If the Accept header can be matched, returns (Just) a tuple of the
    -- Content-Type and response (serialization of @a@ into the appropriate
    -- mimetype).
    handleAcceptH :: Proxy list -> AcceptHeader -> a -> Maybe (ByteString, ByteString)

instance OVERLAPPABLE_
         (Accept ct, AllMime cts, AllMimeRender (ct ': cts) a) => AllCTRender (ct ': cts) a where
    handleAcceptH _ (AcceptHeader accept) val = M.mapAcceptMedia lkup accept
      where pctyps = Proxy :: Proxy (ct ': cts)
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

class AllCTUnrender (list :: [*]) a where
    handleCTypeH :: Proxy list
                 -> ByteString     -- Content-Type header
                 -> ByteString     -- Request body
                 -> Maybe (Either String a)

instance ( AllMimeUnrender ctyps a ) => AllCTUnrender ctyps a where
    handleCTypeH _ ctypeH body = M.mapContentMedia lkup (cs ctypeH)
      where lkup = allMimeUnrender (Proxy :: Proxy ctyps) body

--------------------------------------------------------------------------
-- * Utils (Internal)

class AllMime (list :: [*]) where
    allMime :: Proxy list -> [M.MediaType]

instance AllMime '[] where
    allMime _ = []

instance (Accept ctyp, AllMime ctyps) => AllMime (ctyp ': ctyps) where
    allMime _ = NE.toList (contentTypes pctyp) ++ allMime pctyps
      where
        pctyp  = Proxy :: Proxy ctyp
        pctyps = Proxy :: Proxy ctyps

canHandleAcceptH :: AllMime list => Proxy list -> AcceptHeader -> Bool
canHandleAcceptH p (AcceptHeader h ) = isJust $ M.matchAccept (allMime p) h

--------------------------------------------------------------------------
-- Check that all elements of list are instances of MimeRender
--------------------------------------------------------------------------
class (AllMime list) => AllMimeRender (list :: [*]) a where
    allMimeRender :: Proxy list
                  -> a                              -- value to serialize
                  -> [(M.MediaType, ByteString)]    -- content-types/response pairs

instance OVERLAPPABLE_ ( MimeRender ctyp a ) => AllMimeRender '[ctyp] a where
    allMimeRender _ a = map (, bs) $ NE.toList $ contentTypes pctyp
      where
        bs    = mimeRender pctyp a
        pctyp = Proxy :: Proxy ctyp

instance OVERLAPPABLE_
         ( MimeRender ctyp a
         , AllMimeRender (ctyp' ': ctyps) a
         ) => AllMimeRender (ctyp ': ctyp' ': ctyps) a where
    allMimeRender _ a =
        (map (, bs) $ NE.toList $ contentTypes pctyp)
        ++ allMimeRender pctyps a
      where
        bs     = mimeRender pctyp a
        pctyp  = Proxy :: Proxy ctyp
        pctyps = Proxy :: Proxy (ctyp' ': ctyps)


-- Ideally we would like to declare a 'MimeRender a NoContent' instance, and
-- then this would be taken care of. However there is no more specific instance
-- between that and 'MimeRender JSON a', so we do this instead
instance OVERLAPPING_ ( Accept ctyp ) => AllMimeRender '[ctyp] NoContent where
    allMimeRender _ _ = map (, "") $ NE.toList $ contentTypes pctyp
      where
        pctyp = Proxy :: Proxy ctyp

instance OVERLAPPING_
         ( AllMime (ctyp ': ctyp' ': ctyps)
         ) => AllMimeRender (ctyp ': ctyp' ': ctyps) NoContent where
    allMimeRender p _ = zip (allMime p) (repeat "")

--------------------------------------------------------------------------
-- Check that all elements of list are instances of MimeUnrender
--------------------------------------------------------------------------
class (AllMime list) => AllMimeUnrender (list :: [*]) a where
    allMimeUnrender :: Proxy list
                    -> ByteString
                    -> [(M.MediaType, Either String a)]

instance AllMimeUnrender '[] a where
    allMimeUnrender _ _ = []

instance ( MimeUnrender ctyp a
         , AllMimeUnrender ctyps a
         ) => AllMimeUnrender (ctyp ': ctyps) a where
    allMimeUnrender _ bs =
        (map (, x) $ NE.toList $ contentTypes pctyp)
        ++ allMimeUnrender pctyps bs
      where
        x      = mimeUnrender pctyp bs
        pctyp  = Proxy :: Proxy ctyp
        pctyps = Proxy :: Proxy ctyps

--------------------------------------------------------------------------
-- * MimeRender Instances

-- | `encode`
instance OVERLAPPABLE_
         ToJSON a => MimeRender JSON a where
    mimeRender _ = encode

-- | @urlEncodeAsForm@
-- Note that the @mimeUnrender p (mimeRender p x) == Right x@ law only
-- holds if every element of x is non-null (i.e., not @("", "")@)
instance OVERLAPPABLE_
         ToForm a => MimeRender FormUrlEncoded a where
    mimeRender _ = urlEncodeAsForm

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

-- | A type for responses without content-body.
data NoContent = NoContent
  deriving (Show, Eq, Read, Generic)


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

-- | @urlDecodeAsForm@
-- Note that the @mimeUnrender p (mimeRender p x) == Right x@ law only
-- holds if every element of x is non-null (i.e., not @("", "")@)
instance FromForm a => MimeUnrender FormUrlEncoded a where
    mimeUnrender _ = left TextS.unpack . urlDecodeAsForm

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



-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
