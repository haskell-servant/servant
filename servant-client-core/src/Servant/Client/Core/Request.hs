{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Servant.Client.Core.Request
  ( Request
  , RequestF (..)
  , RequestBody (..)
  , defaultRequest

    -- ** Modifiers
  , addHeader
  , appendToPath
  , appendToQueryString
  , encodeQueryParamValue
  , setQueryString
  , setRequestBody
  , setRequestBodyLBS
  )
where

import Control.DeepSeq (NFData (..))
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..), bifoldMapDefault, bimapDefault)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.HTTP.Media (MediaType)
import Network.HTTP.Types
  ( Header
  , HeaderName
  , HttpVersion (..)
  , Method
  , Query
  , QueryItem
  , http11
  , methodGet
  , urlEncodeBuilder
  )
import Prelude.Compat
import Servant.API (SourceIO, ToHttpApiData, toHeader, toQueryParam)
import Prelude ()

import Servant.Client.Core.Internal (mediaTypeRnf)

data RequestF body path = Request
  { requestPath :: path
  , requestQueryString :: Seq.Seq QueryItem
  , requestBody :: Maybe (body, MediaType)
  , requestAccept :: Seq.Seq MediaType
  , requestHeaders :: Seq.Seq Header
  , requestHttpVersion :: HttpVersion
  , requestMethod :: Method
  }
  deriving (Eq, Foldable, Functor, Generic, Traversable, Typeable)

instance
  (Show a, Show b)
  => Show (Servant.Client.Core.Request.RequestF a b)
  where
  showsPrec p req =
    showParen
      (p >= 11)
      ( showString "Request {requestPath = "
          . showsPrec 0 (requestPath req)
          . showString ", requestQueryString = "
          . showsPrec 0 (requestQueryString req)
          . showString ", requestBody = "
          . showsPrec 0 (requestBody req)
          . showString ", requestAccept = "
          . showsPrec 0 (requestAccept req)
          . showString ", requestHeaders = "
          . showsPrec 0 (redactSensitiveHeader <$> requestHeaders req)
          . showString ", requestHttpVersion = "
          . showsPrec 0 (requestHttpVersion req)
          . showString ", requestMethod = "
          . showsPrec 0 (requestMethod req)
          . showString "}"
      )
    where
      redactSensitiveHeader :: Header -> Header
      redactSensitiveHeader ("Authorization", _) = ("Authorization", "<REDACTED>")
      redactSensitiveHeader h = h

instance Bifunctor RequestF where bimap = bimapDefault

instance Bifoldable RequestF where bifoldMap = bifoldMapDefault

instance Bitraversable RequestF where
  bitraverse f g r =
    mk
      <$> traverse (bitraverse f pure) (requestBody r)
      <*> g (requestPath r)
    where
      mk b p = r{requestBody = b, requestPath = p}

instance (NFData body, NFData path) => NFData (RequestF body path) where
  rnf r =
    rnf (requestPath r) `seq`
      rnf (requestQueryString r) `seq`
        rnfB (requestBody r) `seq`
          rnf (fmap mediaTypeRnf (requestAccept r)) `seq`
            rnf (requestHeaders r) `seq`
              requestHttpVersion r `seq`
                rnf (requestMethod r)
    where
      rnfB Nothing = ()
      rnfB (Just (b, mt)) = rnf b `seq` mediaTypeRnf mt

type Request = RequestF RequestBody Builder

-- | The request body. R replica of the @http-client@ @RequestBody@.
data RequestBody
  = RequestBodyLBS LBS.ByteString
  | RequestBodyBS BS.ByteString
  | RequestBodySource (SourceIO LBS.ByteString)
  deriving (Generic, Typeable)

instance Show RequestBody where
  showsPrec d (RequestBodyLBS lbs) =
    showParen (d > 10) $
      showString "RequestBodyLBS "
        . showsPrec 11 lbs
  showsPrec d (RequestBodyBS bs) =
    showParen (d > 10) $
      showString "RequestBodyBS "
        . showsPrec 11 bs
  showsPrec d (RequestBodySource _) =
    showParen (d > 10) $
      showString "RequestBodySource <IO>"

-- A GET request to the top-level path
defaultRequest :: Request
defaultRequest =
  Request
    { requestPath = ""
    , requestQueryString = Seq.empty
    , requestBody = Nothing
    , requestAccept = Seq.empty
    , requestHeaders = Seq.empty
    , requestHttpVersion = http11
    , requestMethod = methodGet
    }

-- | Append extra path to the request being constructed.
--
-- Warning: This function assumes that the path fragment is already URL-encoded.
appendToPath :: Builder -> Request -> Request
appendToPath p req =
  req{requestPath = requestPath req <> "/" <> p}

-- | Append a query parameter to the request being constructed.
appendToQueryString
  :: Text
  -- ^ query param name
  -> Maybe BS.ByteString
  -- ^ query param value
  -> Request
  -> Request
appendToQueryString pname pvalue req =
  req
    { requestQueryString =
        requestQueryString req
          Seq.|> (encodeUtf8 pname, pvalue)
    }

setQueryString
  :: Query
  -> Request
  -> Request
setQueryString query req =
  req{requestQueryString = Seq.fromList query}

-- | Encode a query parameter value.
encodeQueryParamValue :: ToHttpApiData a => a -> BS.ByteString
encodeQueryParamValue =
  LBS.toStrict
    . Builder.toLazyByteString
    . urlEncodeBuilder True
    . encodeUtf8
    . toQueryParam

-- | Add header to the request being constructed.
addHeader :: ToHttpApiData a => HeaderName -> a -> Request -> Request
addHeader name val req =
  req{requestHeaders = requestHeaders req Seq.|> (name, toHeader val)}

-- | Set body and media type of the request being constructed.
--
-- The body is set to the given bytestring using the 'RequestBodyLBS'
-- constructor.
--
-- @since 0.12
setRequestBodyLBS :: LBS.ByteString -> MediaType -> Request -> Request
setRequestBodyLBS b t req =
  req{requestBody = Just (RequestBodyLBS b, t)}

-- | Set body and media type of the request being constructed.
--
-- @since 0.12
setRequestBody :: RequestBody -> MediaType -> Request -> Request
setRequestBody b t req = req{requestBody = Just (b, t)}
