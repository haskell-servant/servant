{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Servant.Client.Core.Internal.Request where

import           Prelude                 ()
import           Prelude.Compat

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy    as LBS
import           Data.Semigroup          ((<>))
import qualified Data.Sequence           as Seq
import           Data.Text               (Text)
import           Data.Text.Encoding      (encodeUtf8)
import           Data.Typeable           (Typeable)
import           GHC.Generics            (Generic)
import           Network.HTTP.Media      (MediaType)
import           Network.HTTP.Types      (Header, HeaderName, HttpVersion,
                                          Method, QueryItem, Status, http11,
                                          methodGet)
import           Web.HttpApiData         (ToHttpApiData, toEncodedUrlPiece,
                                          toHeader)

-- | A type representing possible errors in a request
--
-- Note that this type substantially changed in 0.12.
data ServantError =
  -- | The server returned an error response
    FailureResponse Response
  -- | The body could not be decoded at the expected type
  | DecodeFailure Text Response
  -- | The content-type of the response is not supported
  | UnsupportedContentType MediaType Response
  -- | The content-type header is invalid
  | InvalidContentTypeHeader Response
  -- | There was a connection error, and no response was received
  | ConnectionError Text
  deriving (Eq, Show, Generic, Typeable)

data RequestF a = Request
  { requestPath        :: a
  , requestQueryString :: Seq.Seq QueryItem
  , requestBody        :: Maybe (RequestBody, MediaType)
  , requestAccept      :: Seq.Seq MediaType
  , requestHeaders     :: Seq.Seq Header
  , requestHttpVersion :: HttpVersion
  , requestMethod      :: Method
  } deriving (Eq, Show, Functor, Generic, Typeable)

type Request = RequestF Builder.Builder

-- | The request body. Currently only lazy ByteStrings are supported.
newtype RequestBody = RequestBodyLBS LBS.ByteString
  deriving (Eq, Ord, Read, Show, Typeable)

data Response = Response
  { responseStatusCode  :: Status
  , responseBody        :: LBS.ByteString
  , responseHeaders     :: Seq.Seq Header
  , responseHttpVersion :: HttpVersion
  } deriving (Eq, Show, Generic, Typeable)

-- A GET request to the top-level path
defaultRequest :: Request
defaultRequest = Request
  { requestPath = ""
  , requestQueryString = Seq.empty
  , requestBody = Nothing
  , requestAccept = Seq.empty
  , requestHeaders = Seq.empty
  , requestHttpVersion = http11
  , requestMethod = methodGet
  }

appendToPath :: Text -> Request -> Request
appendToPath p req
  = req { requestPath = requestPath req <> "/" <> toEncodedUrlPiece p }

appendToQueryString :: Text       -- ^ param name
                    -> Maybe Text -- ^ param value
                    -> Request
                    -> Request
appendToQueryString pname pvalue req
  = req { requestQueryString = requestQueryString req
                        Seq.|> (encodeUtf8 pname, encodeUtf8 <$> pvalue)}

addHeader :: ToHttpApiData a => HeaderName -> a -> Request -> Request
addHeader name val req
  = req { requestHeaders = requestHeaders req Seq.|> (name, toHeader val)}

-- | Set body and media type of the request being constructed.
--
-- The body is set to the given bytestring using the 'RequestBodyLBS'
-- constructor.
--
-- @since 0.12
--
setRequestBodyLBS :: LBS.ByteString -> MediaType -> Request -> Request
setRequestBodyLBS b t req
  = req { requestBody = Just (RequestBodyLBS b, t) }

-- | Set body and media type of the request being constructed.
--
-- @since 0.12
--
setRequestBody :: RequestBody -> MediaType -> Request -> Request
setRequestBody b t req = req { requestBody = Just (b, t) }
