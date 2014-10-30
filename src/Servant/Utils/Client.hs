module Servant.Utils.Client where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson
import Data.String.Conversions
import Network.HTTP.Types
import Network.URI
import Servant.Client

import qualified Network.HTTP.Client as Client

performRequest :: FromJSON result =>
  Method -> Req -> Int -> URIAuth -> EitherT String IO result
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
      result <- maybe (left (requestString ++ " returned invalid json")) return $
        decode' (Client.responseBody response)
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
