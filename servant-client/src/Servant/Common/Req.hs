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
import Control.Monad
import Control.Monad.Catch (MonadThrow, MonadCatch)
import Data.Foldable (toList)
import Data.Functor.Alt (Alt (..))
import Data.Semigroup ((<>))

import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Trans.Except

import GHC.Generics
import Control.Monad.Base (MonadBase (..))
import Control.Monad.IO.Class ()
import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl (..))
import qualified Data.ByteString.Builder as BS
import Data.ByteString.Lazy hiding (pack, filter, map, null, elem, any)
import qualified Data.ByteString.Lazy as BS
import Data.String
import Data.String.Conversions (cs)
import Data.Proxy
import Data.Text (Text)
import Data.Text.Encoding
import Data.Typeable
import Network.HTTP.Media
import Network.HTTP.Types
import Network.HTTP.Client hiding (Proxy, path)
import qualified Network.HTTP.Types.Header   as HTTP
import Network.URI hiding (path)
import Servant.API.ContentTypes
import Servant.Common.BaseUrl

import qualified Network.HTTP.Client as Client

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
        fullPath = path ++ cs (slashIfNull $ BS.toLazyByteString (reqPath req))
          where slashIfNull s | BS.null s = "/"
                              | otherwise = s

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

data ClientEnv
  = ClientEnv
  { manager :: Manager
  , baseUrl :: BaseUrl
  }


-- | @ClientM@ is the monad in which client functions run. Contains the
-- 'Manager' and 'BaseUrl' used for requests in the reader environment.

newtype ClientM a = ClientM { runClientM' :: ReaderT ClientEnv (ExceptT ServantError IO) a }
                    deriving ( Functor, Applicative, Monad, MonadIO, Generic
                             , MonadReader ClientEnv
                             , MonadError ServantError
                             , MonadThrow, MonadCatch
                             )

instance MonadBase IO ClientM where
  liftBase = ClientM . liftBase

instance MonadBaseControl IO ClientM where
  type StM ClientM a = Either ServantError a

  -- liftBaseWith :: (RunInBase ClientM IO -> IO a) -> ClientM a
  liftBaseWith f = ClientM (liftBaseWith (\g -> f (g . runClientM')))

  -- restoreM :: StM ClientM a -> ClientM a
  restoreM st = ClientM (restoreM st)

-- | Try clients in order, last error is preserved.
instance Alt ClientM where
  a <!> b = a `catchError` \_ -> b

runClientM :: ClientM a -> ClientEnv -> IO (Either ServantError a)
runClientM cm env = runExceptT $ (flip runReaderT env) $ runClientM' cm


performRequest :: Method -> Req
               -> ClientM ( Int, ByteString, MediaType
                          , [HTTP.Header], Response ByteString)
performRequest reqMethod req = do
  m <- asks manager
  reqHost <- asks baseUrl
  partialRequest <- liftIO $ reqToRequest req reqHost

  let request = partialRequest { Client.method = reqMethod }

  eResponse <- liftIO $ catchConnectionError $ Client.httpLbs request m
  case eResponse of
    Left err ->
      throwError . ConnectionError $ SomeException err

    Right response -> do
      let status = Client.responseStatus response
          body = Client.responseBody response
          hdrs = Client.responseHeaders response
          status_code = statusCode status
      ct <- case lookup "Content-Type" $ Client.responseHeaders response of
                 Nothing -> pure $ "application"//"octet-stream"
                 Just t -> case parseAccept t of
                   Nothing -> throwError $ InvalidContentTypeHeader (cs t) body
                   Just t' -> pure t'
      unless (status_code >= 200 && status_code < 300) $
        throwError $ FailureResponse (UrlReq reqHost req) status ct body
      return (status_code, body, ct, hdrs, response)

performRequestCT :: MimeUnrender ct result => Proxy ct -> Method -> Req
    -> ClientM ([HTTP.Header], result)
performRequestCT ct reqMethod req = do
  let acceptCTS = contentTypes ct
  (_status, respBody, respCT, hdrs, _response) <-
    performRequest reqMethod (req { reqAccept = toList acceptCTS })
  unless (any (matches respCT) acceptCTS) $ throwError $ UnsupportedContentType respCT respBody
  case mimeUnrender ct respBody of
    Left err -> throwError $ DecodeFailure err respCT respBody
    Right val -> return (hdrs, val)

performRequestNoBody :: Method -> Req -> ClientM [HTTP.Header]
performRequestNoBody reqMethod req = do
  (_status, _body, _ct, hdrs, _response) <- performRequest reqMethod req
  return hdrs

catchConnectionError :: IO a -> IO (Either ServantError a)
catchConnectionError action =
  catch (Right <$> action) $ \e ->
    pure . Left . ConnectionError $ SomeException (e :: HttpException)
