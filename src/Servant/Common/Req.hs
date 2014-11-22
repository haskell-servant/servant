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
import Data.ByteString.Lazy
import Data.String.Conversions
import Data.Text
import Network.HTTP.Client
import Network.HTTP.Types
import Network.URI
import Servant.Common.BaseUrl
import System.IO.Unsafe

import qualified Network.HTTP.Client as Client

data Req = Req
  { reqPath  :: String
  , qs       :: QueryText
  , reqBody  :: ByteString
  }

defReq :: Req
defReq = Req "" [] ""

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

setRQBody :: ByteString -> Req -> Req
setRQBody b req = req { reqBody = b }

reqToRequest :: (Functor m, MonadThrow m) => Req -> BaseUrl -> m Request
reqToRequest req (BaseUrl reqScheme reqHost reqPort) = fmap (setrqb . setQS ) $ parseUrl url

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

        setrqb r = r { requestBody = RequestBodyLBS (reqBody req) }
        setQS = setQueryString $ queryTextToQuery (qs req)


-- * performing requests

{-# NOINLINE __manager #-}
__manager :: MVar Manager
__manager = unsafePerformIO (newManager defaultManagerSettings >>= newMVar)

__withGlobalManager :: (Manager -> IO a) -> IO a
__withGlobalManager action = modifyMVar __manager $ \ manager -> do
  result <- action manager
  return (manager, result)


displayHttpRequest :: Method -> String
displayHttpRequest httpmethod = "HTTP " ++ cs httpmethod ++ " request"


performRequest :: Method -> Req -> (Int -> Bool) -> BaseUrl -> EitherT String IO (Int, ByteString)
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
      return $ (statusCode status, Client.responseBody response)
  where
    showStatus (Status code message) =
      show code ++ " - " ++ cs message


performRequestJSON :: FromJSON result =>
  Method -> Req -> Int -> BaseUrl -> EitherT String IO result
performRequestJSON reqMethod req wantedStatus reqHost = do
  (_status, respBody) <- performRequest reqMethod req (== wantedStatus) reqHost
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
