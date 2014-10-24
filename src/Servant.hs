{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant where

import Control.Applicative
import Control.Concurrent (forkIO, killThread)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Monoid
import Data.Proxy
import Data.Text
import GHC.Generics
import GHC.TypeLits
import Network.HTTP.Types
import Network.URI
import Network.Wai
import Network.Wai.Handler.Warp
import Soenke

import qualified Network.HTTP.Client as Http.Client

-- * Captures
data Capture sym a

class FromText a where
  capture :: Text -> Maybe a

class ToText a where
  toText :: a -> Text

instance FromText Text where
  capture = Just

instance ToText Text where
  toText = id

captured :: FromText a => proxy (Capture sym a) -> Text -> Maybe a
captured _ = capture

instance (KnownSymbol capture, FromText a, HasServer sublayout)
      => HasServer (Capture capture a :> sublayout) where

  type Server (Capture capture a :> sublayout) =
     a -> Server sublayout

  route Proxy subserver request = case pathInfo request of
    (first : rest)
      -> case captured captureProxy first of
           Nothing  -> return Nothing
           Just v   -> route (Proxy :: Proxy sublayout) (subserver v) request{
                         pathInfo = rest
                       }
    _ -> return Nothing

    where captureProxy = Proxy :: Proxy (Capture capture a)

instance (KnownSymbol capture, ToText a, HasClient sublayout)
      => HasClient (Capture capture a :> sublayout) where

  type Client (Capture capture a :> sublayout) =
    a -> Client sublayout

  clientWithRoute Proxy path val =
      clientWithRoute (Proxy :: Proxy sublayout)
                      (path ++ "/" ++ unpack (toText val))

-- * Request Body support
data RQBody a

instance (FromJSON a, HasServer sublayout)
      => HasServer (RQBody a :> sublayout) where

  type Server (RQBody a :> sublayout) =
    a -> Server sublayout

  route Proxy subserver request = do
    mrqbody <- decode' <$> lazyRequestBody request
    case mrqbody of
      Nothing -> return Nothing
      Just v  -> route (Proxy :: Proxy sublayout) (subserver v) request

instance (ToJSON a, FromJSON b)
      => HasClient (RQBody a :> Post b) where

  type Client (RQBody a :> Post b) =
    a -> URI -> EitherT String IO b

  clientWithRoute Proxy path body uri = do
    partialRequest <- liftIO . Http.Client.parseUrl $
      show ( nullURI { uriPath = path }
             `relativeTo` uri
           )

    let request = partialRequest
          { Http.Client.method = methodPost
          , Http.Client.requestBody = Http.Client.RequestBodyLBS (encode body)
          }

    innerResponse <- liftIO . __withGlobalManager $ \ manager ->
      Http.Client.httpLbs request manager

    when (Http.Client.responseStatus innerResponse /= status201) $
      left ("HTTP POST request failed with status: " ++ show (Http.Client.responseStatus innerResponse))

    maybe (left "HTTP POST request returned invalid json") return $
      decode' (Http.Client.responseBody innerResponse)

-- * Example

type TestApi = "hello" :> Capture "name" Text :> Get Greet
          :<|> "greet" :> RQBody Greet :> Post Greet

testApi :: Proxy TestApi
testApi = Proxy

data Greet = Greet { msg :: Text }
  deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet

server :: Server TestApi
server = hello :<|> greet

  where hello = return . Greet . ("Hello, " <>)
        greet = return

getGreet :: Text -> URI -> EitherT String IO Greet
postGreet :: Greet -> URI -> EitherT String IO Greet

getGreet :<|> postGreet
  = client testApi

test :: Application
test = serve testApi server

runTestServer :: Port -> IO ()
runTestServer port = run port test

runTest :: IO ()
runTest = do
  tid <- forkIO $ runTestServer 8001
  let Just uri = parseURI "http://localhost:8001/"
  print =<< runEitherT (getGreet "alp" uri)
  let g = Greet "yo"
  print =<< runEitherT (postGreet g uri)
  killThread tid
