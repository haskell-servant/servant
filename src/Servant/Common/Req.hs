{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.Common.Req where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Aeson.Parser
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString.Lazy hiding (pack)
import Data.String
import Data.String.Conversions
import Data.Text
import Data.Text.Encoding
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Media
import Network.HTTP.Types
import Network.URI
import Servant.Common.BaseUrl
import Servant.Common.Text
import System.IO.Unsafe

import qualified Network.HTTP.Client as Client

data Req = Req
  { reqPath  :: String
  , qs       :: QueryText
  , reqBody  :: Maybe (ByteString, MediaType)
  , headers  :: [(String, Text)]
  }

defReq :: Req
defReq = Req "" [] Nothing []

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
reqToRequest req (BaseUrl reqScheme reqHost reqPort) =
    fmap (setheaders . setrqb . setQS ) $ parseUrl url

  where url = show $ nullURI { uriScheme = case reqScheme of
                                  Http  -> "http:"
                                  Https -> "https:"
                             , uriAuthority = Just $
                                 URIAuth { uriUserInfo = ""
                                         , uriRegName = reqHost
                                         , uriPort = ":" ++ show reqPort
                                         }
                             , uriPath = reqPath req
                             }

        setrqb r = case (reqBody req) of
                     Nothing -> r
                     Just (b,t) -> r { requestBody = RequestBodyLBS b
                                     , requestHeaders = [(hContentType, cs . show $ t)] }
        setQS = setQueryString $ queryTextToQuery (qs req)
        setheaders r = r { requestHeaders = requestHeaders r
                                         ++ Prelude.map toProperHeader (headers req) }

        toProperHeader (name, val) =
          (fromString name, encodeUtf8 val)


-- * performing requests

{-# NOINLINE __manager #-}
__manager :: MVar Manager
__manager = unsafePerformIO (newManager tlsManagerSettings >>= newMVar)

__withGlobalManager :: (Manager -> IO a) -> IO a
__withGlobalManager action = modifyMVar __manager $ \ manager -> do
  result <- action manager
  return (manager, result)


displayHttpRequest :: Method -> String
displayHttpRequest httpmethod = "HTTP " ++ cs httpmethod ++ " request"


performRequest :: Method -> Req -> (Int -> Bool) -> BaseUrl -> EitherT String IO (Int, ByteString, MediaType)
performRequest reqMethod req isWantedStatus reqHost = do
  partialRequest <- liftIO $ reqToRequest req reqHost

  let request = partialRequest { Client.method = reqMethod
                               , checkStatus = \ _status _headers _cookies -> Nothing
                               }

  eResponse <- liftIO $ __withGlobalManager $ \ manager ->
    catchStatusCodeException $
    Client.httpLbs request manager
  case eResponse of
    Left status ->
      left (displayHttpRequest reqMethod ++ " failed with status: " ++ showStatus status)

    Right response -> do
      let status = Client.responseStatus response
      unless (isWantedStatus (statusCode status)) $
        left (displayHttpRequest reqMethod ++ " failed with status: " ++ showStatus status)
      ct <- case lookup "Content-Type" $ Client.responseHeaders response of
                 Nothing -> pure $ "application"//"octet-stream"
                 Just t -> case parseAccept t of
                   Nothing -> left $ "invalid Content-Type header: " <> cs t
                   Just t' -> pure t'
      return $ (statusCode status, Client.responseBody response, ct)
  where
    showStatus (Status code message) =
      show code ++ " - " ++ cs message


performRequestJSON :: FromJSON result =>
  Method -> Req -> Int -> BaseUrl -> EitherT String IO result
performRequestJSON reqMethod req wantedStatus reqHost = do
  (_status, respBody, _) <- performRequest reqMethod req (== wantedStatus) reqHost
  either
    (\ message -> left (displayHttpRequest reqMethod ++ " returned invalid json: " ++ message))
    return
    (decodeLenient respBody)


catchStatusCodeException :: IO a -> IO (Either Status a)
catchStatusCodeException action =
  catch (Right <$> action) $ \e ->
    case e of
      Client.StatusCodeException status _ _ -> return $ Left status
      exc -> throwIO exc

-- | Like 'Data.Aeson.decode' but allows all JSON values instead of just
-- objects and arrays.
decodeLenient :: FromJSON a => ByteString -> Either String a
decodeLenient input = do
  v :: Value <- parseOnly (Data.Aeson.Parser.value <* endOfInput) (cs input)
  parseEither parseJSON v
