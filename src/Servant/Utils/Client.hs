{-# LANGUAGE ScopedTypeVariables #-}
module Servant.Utils.Client where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Aeson.Parser
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString.Lazy
import Data.String.Conversions
import Network.HTTP.Types
import Servant.Client
import Servant.Client.BaseUrl

import qualified Network.HTTP.Client as Client

performRequest :: FromJSON result =>
  Method -> Req -> Int -> BaseUrl -> EitherT String IO result
performRequest method req wantedStatus host = do
  partialRequest <- liftIO $ reqToRequest req host

  let request = partialRequest { Client.method = method
                               }

  eResponse <- liftIO $ __withGlobalManager $ \ manager ->
    catchStatusCodeException $
    Client.httpLbs request manager
  case eResponse of
    Left status ->
      left (requestString ++ " failed with status: " ++ showStatus status)

    Right response -> do
      let status = Client.responseStatus response
      when (statusCode status /= wantedStatus) $
        left (requestString ++ " failed with status: " ++ showStatus status)
      result <- either
        (\ message -> left (requestString ++ " returned invalid json: " ++ message))
        return
        (decodeLenient (Client.responseBody response))
      return result
  where
    requestString = "HTTP " ++ cs method ++ " request"
    showStatus (Status code message) =
      show code ++ " - " ++ cs message

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
