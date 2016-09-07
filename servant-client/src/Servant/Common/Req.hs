{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}

module Servant.Common.Req where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Exception
import Control.Monad
import Control.Monad.Catch (MonadThrow)

#if MIN_VERSION_mtl(2,2,0)
import Control.Monad.Except (MonadError(..))
#else
import Control.Monad.Error.Class (MonadError(..))
#endif
import Control.Monad.Trans.Except


import GHC.Generics
import Control.Monad.IO.Class ()
import Control.Monad.Reader
import Data.ByteString.Lazy hiding (pack, filter, map, null, elem)
import Data.String
import Data.String.Conversions
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
    { responseStatus            :: Status
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
  FailureResponse a b c == FailureResponse x y z =
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

data Req = Req
  { reqPath   :: String
  , qs        :: QueryText
  , reqBody   :: Maybe (ByteString, MediaType)
  , reqAccept :: [MediaType]
  , headers   :: [(String, Text)]
  }

defReq :: Req
defReq = Req "" [] Nothing [] []

appendToPath :: String -> Req -> Req
appendToPath p req =
  req { reqPath = reqPath req ++ "/" ++ p }

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

setRQBody :: ByteString -> MediaType -> Req -> Req
setRQBody b t req = req { reqBody = Just (b, t) }

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
                             , uriPath = path ++ reqPath req
                             }

        setrqb r = case reqBody req of
                     Nothing -> r
                     Just (b,t) -> r { requestBody = RequestBodyLBS b
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
                             )

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
        throwError $ FailureResponse status ct body
      return (status_code, body, ct, hdrs, response)

performRequestCT :: MimeUnrender ct result => Proxy ct -> Method -> Req 
    -> ClientM ([HTTP.Header], result)
performRequestCT ct reqMethod req = do
  let acceptCT = contentType ct
  (_status, respBody, respCT, hdrs, _response) <-
    performRequest reqMethod (req { reqAccept = [acceptCT] })
  unless (matches respCT (acceptCT)) $ throwError $ UnsupportedContentType respCT respBody
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
