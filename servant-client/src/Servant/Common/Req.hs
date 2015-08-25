{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.Common.Req where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Exception
import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.ByteString.Lazy hiding (pack, filter, map, null, elem)
import Data.IORef
import Data.String
import Data.String.Conversions
import Data.Proxy
import Data.Text (Text)
import Data.Text.Encoding
import Data.Typeable
import Network.HTTP.Client hiding (Proxy, path)
import Network.HTTP.Client.TLS
import Network.HTTP.Media
import Network.HTTP.Types
import qualified Network.HTTP.Types.Header   as HTTP
import Network.URI hiding (path)
import Servant.API.ContentTypes
import Servant.Common.BaseUrl
import Servant.Common.Text
import System.IO.Unsafe

import qualified Network.HTTP.Client as Client

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

appendToMatrixParams :: String
                     -> Maybe String
                     -> Req
                     -> Req
appendToMatrixParams pname pvalue req =
  req { reqPath = reqPath req ++ ";" ++ pname ++ maybe "" ("=" ++) pvalue }

appendToQueryString :: Text       -- ^ param name
                    -> Maybe Text -- ^ param value
                    -> Req
                    -> Req
appendToQueryString pname pvalue req =
  req { qs = qs req ++ [(pname, pvalue)]
      }

addHeader :: ToText a => String -> a -> Req -> Req
addHeader name val req = req { headers = headers req
                                      ++ [(name, toText val)]
                             }

setRQBody :: ByteString -> MediaType -> Req -> Req
setRQBody b t req = req { reqBody = Just (b, t) }

reqToRequest :: (Functor m, MonadThrow m) => Req -> BaseUrl -> m Request
reqToRequest req (BaseUrl reqScheme reqHost reqPort path) =
    setheaders . setAccept . setrqb . setQS <$> parseUrl url

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


-- * performing requests

{-# NOINLINE __manager #-}
__manager :: IORef Manager
__manager = unsafePerformIO (newManager tlsManagerSettings >>= newIORef)

__withGlobalManager :: (Manager -> IO a) -> IO a
__withGlobalManager action = readIORef __manager >>= action


displayHttpRequest :: Method -> String
displayHttpRequest httpmethod = "HTTP " ++ cs httpmethod ++ " request"


performRequest :: Method -> Req -> (Int -> Bool) -> BaseUrl
               -> EitherT ServantError IO ( Int, ByteString, MediaType
                                          , [HTTP.Header], Response ByteString)
performRequest reqMethod req isWantedStatus reqHost = do
  partialRequest <- liftIO $ reqToRequest req reqHost

  let request = partialRequest { Client.method = reqMethod
                               , checkStatus = \ _status _headers _cookies -> Nothing
                               }

  eResponse <- liftIO $ __withGlobalManager $ \ manager ->
    catchConnectionError $
      Client.httpLbs request manager
  case eResponse of
    Left err ->
      left . ConnectionError $ SomeException err

    Right response -> do
      let status = Client.responseStatus response
          body = Client.responseBody response
          hrds = Client.responseHeaders response
          status_code = statusCode status
      ct <- case lookup "Content-Type" $ Client.responseHeaders response of
                 Nothing -> pure $ "application"//"octet-stream"
                 Just t -> case parseAccept t of
                   Nothing -> left $ InvalidContentTypeHeader (cs t) body
                   Just t' -> pure t'
      unless (isWantedStatus status_code) $
        left $ FailureResponse status ct body
      return (status_code, body, ct, hrds, response)


performRequestCT :: MimeUnrender ct result =>
  Proxy ct -> Method -> Req -> [Int] -> BaseUrl -> EitherT ServantError IO ([HTTP.Header], result)
performRequestCT ct reqMethod req wantedStatus reqHost = do
  let acceptCT = contentType ct
  (_status, respBody, respCT, hrds, _response) <-
    performRequest reqMethod (req { reqAccept = [acceptCT] }) (`elem` wantedStatus) reqHost
  unless (matches respCT (acceptCT)) $ left $ UnsupportedContentType respCT respBody
  case mimeUnrender ct respBody of
    Left err -> left $ DecodeFailure err respCT respBody
    Right val -> return (hrds, val)

performRequestNoBody :: Method -> Req -> [Int] -> BaseUrl -> EitherT ServantError IO ()
performRequestNoBody reqMethod req wantedStatus reqHost = do
  _ <- performRequest reqMethod req (`elem` wantedStatus) reqHost
  return ()

catchConnectionError :: IO a -> IO (Either ServantError a)
catchConnectionError action =
  catch (Right <$> action) $ \e ->
    pure . Left . ConnectionError $ SomeException (e :: HttpException)
