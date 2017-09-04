{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Servant.Common.Req where

import Prelude ()
import Prelude.Compat

import Control.Exception
import Control.Monad.Catch (MonadThrow)
import Data.Semigroup ((<>))

import Control.Monad.IO.Class ()
import qualified Data.ByteString.Builder as BS
import Data.ByteString.Lazy hiding (pack, filter, map, null, elem, any)
import Data.String
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text.Encoding
import Data.Typeable
import Network.HTTP.Media
import Network.HTTP.Types
import Network.HTTP.Client hiding (Proxy, path)
import Network.URI hiding (path)
import Servant.Common.BaseUrl

import Web.HttpApiData

data ServantError
  = FailureResponse
    { failingRequest            :: UrlReq
    , responseStatus            :: Status
    , responseContentType       :: MediaType
    , responseBody              :: ByteString
    }
  | DecodeFailure
    { decodeError               :: String
    , responseContentType       :: MediaType
    , responseBody              :: ByteString
    }
  | UnsupportedContentType
    { responseContentType       :: MediaType
    , responseBody              :: ByteString
    }
  | InvalidContentTypeHeader
    { responseContentTypeHeader :: ByteString
    , responseBody              :: ByteString
    }
  | ConnectionError
    { connectionError           :: SomeException
    }
  deriving (Show, Typeable)

instance Eq ServantError where
  FailureResponse _ a b c == FailureResponse _ x y z =
    (a, b, c) == (x, y, z)
  DecodeFailure a b c == DecodeFailure x y z =
    (a, b, c) == (x, y, z)
  UnsupportedContentType a b == UnsupportedContentType x y =
    (a, b) == (x, y)
  InvalidContentTypeHeader a b == InvalidContentTypeHeader x y =
    (a, b) == (x, y)
  ConnectionError a == ConnectionError x =
    show a == show x
  _ == _ = False

instance Exception ServantError

data UrlReq = UrlReq BaseUrl Req

instance Show UrlReq where
  show (UrlReq url req) = showBaseUrl url ++ path ++ "?" ++ show (qs req)
    where
      path = cs (BS.toLazyByteString (reqPath req))

data Req = Req
  { reqPath   :: BS.Builder
  , qs        :: QueryText
  , reqBody   :: Maybe (RequestBody, MediaType)
  , reqAccept :: [MediaType]
  , headers   :: [(String, Text)]
  }

defReq :: Req
defReq = Req "" [] Nothing [] []

appendToPath :: String -> Req -> Req
appendToPath p req =
  req { reqPath = reqPath req <> "/" <> toEncodedUrlPiece p }

appendToQueryString :: Text       -- ^ param name
                    -> Maybe Text -- ^ param value
                    -> Req
                    -> Req
appendToQueryString pname pvalue req =
  req { qs = qs req ++ [(pname, pvalue)]
      }

addHeader :: ToHttpApiData a => String -> a -> Req -> Req
addHeader name val req = req { headers = headers req
                                      ++ [(name, decodeUtf8 (toHeader val))]
                             }

-- | Set body and media type of the request being constructed.
--
-- The body is set to the given bytestring using the 'RequestBodyLBS'
-- constructor.
--
{-# DEPRECATED setRQBody "Use setReqBodyLBS instead" #-}
setRQBody :: ByteString -> MediaType -> Req -> Req
setRQBody = setReqBodyLBS

-- | Set body and media type of the request being constructed.
--
-- The body is set to the given bytestring using the 'RequestBodyLBS'
-- constructor.
--
-- @since 0.9.2.0
--
setReqBodyLBS :: ByteString -> MediaType -> Req -> Req
setReqBodyLBS b t req = req { reqBody = Just (RequestBodyLBS b, t) }

-- | Set body and media type of the request being constructed.
--
-- @since 0.9.2.0
--
setReqBody :: RequestBody -> MediaType -> Req -> Req
setReqBody b t req = req { reqBody = Just (b, t) }

reqToRequest :: (Functor m, MonadThrow m) => Req -> BaseUrl -> m Request
reqToRequest req (BaseUrl reqScheme reqHost reqPort path) =
    setheaders . setAccept . setrqb . setQS <$> parseRequest url

  where url = show $ nullURI { uriScheme = case reqScheme of
                                  Http  -> "http:"
                                  Https -> "https:"
                             , uriAuthority = Just $
                                 URIAuth { uriUserInfo = ""
                                         , uriRegName = reqHost
                                         , uriPort = ":" ++ show reqPort
                                         }
                             , uriPath = fullPath
                             }
        fullPath = path ++ cs (BS.toLazyByteString (reqPath req))

        setrqb r = case reqBody req of
                     Nothing -> r
                     Just (b,t) -> r { requestBody = b
                                     , requestHeaders = requestHeaders r
                                                     ++ [(hContentType, cs . show $ t)] }
        setQS = setQueryString $ queryTextToQuery (qs req)
        setheaders r = r { requestHeaders = requestHeaders r
                                         <> fmap toProperHeader (headers req) }
        setAccept r = r { requestHeaders = filter ((/= "Accept") . fst) (requestHeaders r)
                                        <> [("Accept", renderHeader $ reqAccept req)
                                              | not . null . reqAccept $ req] }
        toProperHeader (name, val) =
          (fromString name, encodeUtf8 val)

#if !MIN_VERSION_http_client(0,4,30)
-- 'parseRequest' is introduced in http-client-0.4.30
-- it differs from 'parseUrl', by not throwing exceptions on non-2xx http statuses
--
-- See for implementations:
-- http://hackage.haskell.org/package/http-client-0.4.30/docs/src/Network-HTTP-Client-Request.html#parseRequest
-- http://hackage.haskell.org/package/http-client-0.5.0/docs/src/Network-HTTP-Client-Request.html#parseRequest
parseRequest :: MonadThrow m => String -> m Request
parseRequest url = liftM disableStatusCheck (parseUrl url)
  where
    disableStatusCheck req = req { checkStatus = \ _status _headers _cookies -> Nothing }
#endif


-- * performing requests

displayHttpRequest :: Method -> String
displayHttpRequest httpmethod = "HTTP " ++ cs httpmethod ++ " request"
