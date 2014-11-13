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
reqToRequest req (BaseUrl scheme host port) = fmap (setrqb . setQS ) $ parseUrl url

  where url = show $ nullURI { uriScheme = case scheme of
                                  Http  -> "http:"
                                  Https -> "https:"
                             , uriAuthority = Just $
                                 URIAuth { uriUserInfo = ""
                                         , uriRegName = host
                                         , uriPort = ":" ++ show port
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
displayHttpRequest method = "HTTP " ++ cs method ++ " request"


performRequest :: Method -> Req -> (Int -> Bool) -> BaseUrl -> EitherT String IO ByteString
performRequest method req isWantedStatus host = do
  partialRequest <- liftIO $ reqToRequest req host

  let request = partialRequest { Client.method = method
                               }

  eResponse <- liftIO $ __withGlobalManager $ \ manager ->
    catchStatusCodeException $
    Client.httpLbs request manager
  case eResponse of
    Left status ->
      left (displayHttpRequest method ++ " failed with status: " ++ showStatus status)

    Right response -> do
      let status = Client.responseStatus response
      unless (isWantedStatus (statusCode status)) $
        left (displayHttpRequest method ++ " failed with status: " ++ showStatus status)
      return $ Client.responseBody response
  where
    showStatus (Status code message) =
      show code ++ " - " ++ cs message


performRequestJSON :: FromJSON result =>
  Method -> Req -> Int -> BaseUrl -> EitherT String IO result
performRequestJSON method req wantedStatus host = do
  responseBody <- performRequest method req (== wantedStatus) host
  either
    (\ message -> left (displayHttpRequest method ++ " returned invalid json: " ++ message))
    return
    (decodeLenient responseBody)


catchStatusCodeException :: IO a -> IO (Either Status a)
catchStatusCodeException action = catch (Right <$> action) $
  \ e -> case e of
    Client.StatusCodeException status _ _ ->
      return $ Left status
    e -> throwIO e

-- | Like 'Data.Aeson.decode' but allows all JSON values instead of just
-- objects and arrays.
decodeLenient :: FromJSON a => ByteString -> Either String a
decodeLenient input = do
  v :: Value <- parseOnly (Data.Aeson.Parser.value <* endOfInput) (cs input)
  parseEither parseJSON v
