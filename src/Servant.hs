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
import Data.String.Conversions
import Data.Text
import GHC.Generics
import GHC.TypeLits
import Network.HTTP.Types
import Network.URI
import Network.Wai
import Network.Wai.Handler.Warp
import Soenke

import qualified Network.HTTP.Client as Http.Client

class FromText a where
  fromText :: Text -> Maybe a

class ToText a where
  toText :: a -> Text

instance FromText Text where
  fromText = Just

instance ToText Text where
  toText = id

instance FromText Bool where
  fromText "true"  = Just True
  fromText "false" = Just False
  fromText _       = Nothing

instance ToText Bool where
  toText True  = "true"
  toText False = "false"

-- * Captures
data Capture sym a

captured :: FromText a => proxy (Capture sym a) -> Text -> Maybe a
captured _ = fromText

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

  clientWithRoute Proxy req val =
    clientWithRoute (Proxy :: Proxy sublayout) $
      appendToPath p req

    where p = unpack (toText val)

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

instance (ToJSON a, HasClient sublayout)
      => HasClient (RQBody a :> sublayout) where

  type Client (RQBody a :> sublayout) =
    a -> Client sublayout

  clientWithRoute Proxy req body =
    clientWithRoute (Proxy :: Proxy sublayout) $
      setRQBody (encode body) req

-- * GET params support (i.e query string arguments)
data GetParam sym a

instance (KnownSymbol sym, FromText a, HasServer sublayout)
      => HasServer (GetParam sym a :> sublayout) where

  type Server (GetParam sym a :> sublayout) =
    Maybe a -> Server sublayout

  route Proxy subserver request = do
    let querytext = parseQueryText $ rawQueryString request
        param =
          case lookup paramName querytext of
            Nothing       -> Nothing -- param absent from the query string
            Just Nothing  -> Nothing -- param present with no value -> Nothing
            Just (Just v) -> fromText v -- if present, we try to convert to
                                        -- the right type

    route (Proxy :: Proxy sublayout) (subserver param) request

    where paramName = cs $ symbolVal (Proxy :: Proxy sym)

instance (KnownSymbol sym, ToText a, HasClient sublayout)
      => HasClient (GetParam sym a :> sublayout) where

  type Client (GetParam sym a :> sublayout) =
    Maybe a -> Client sublayout

  -- if mparam = Nothing, we don't add it to the query string
  clientWithRoute Proxy req mparam =
    clientWithRoute (Proxy :: Proxy sublayout) $
      appendToQueryString pname mparamText req

    where pname  = pack pname'
          pname' = symbolVal (Proxy :: Proxy sym)
          mparamText = fmap toText mparam

-- * Example

data Greet = Greet { msg :: Text }
  deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet

-- API specification
type TestApi = 
       "hello" :> Capture "name" Text :> GetParam "capital" Bool :> Get Greet
  :<|> "greet" :> RQBody Greet :> Post Greet

testApi :: Proxy TestApi
testApi = Proxy

-- Server-side handlers
server :: Server TestApi
server = hello :<|> greet

  where hello name Nothing = hello name (Just False)
        hello name (Just False) = return . Greet $ "Hello, " <> name
        hello name (Just True) = return . Greet . toUpper $ "Hello, " <> name

        greet = return

-- Client-side query functions
clientApi :: Client TestApi
clientApi = client testApi

getGreet :: Text -> Maybe Bool -> URI -> EitherT String IO Greet
postGreet :: Greet -> URI -> EitherT String IO Greet
getGreet :<|> postGreet = clientApi

-- Turn the server into a WAI app
test :: Application
test = serve testApi server

-- Run the server
runTestServer :: Port -> IO ()
runTestServer port = run port test

-- Run some queries against the server
runTest :: IO ()
runTest = do
  tid <- forkIO $ runTestServer 8001
  let Just uri = parseURI "http://localhost:8001"
  print =<< runEitherT (getGreet "alp" (Just True) uri)
  print =<< runEitherT (getGreet "alp" (Just False) uri)
  let g = Greet "yo"
  print =<< runEitherT (postGreet g uri)
  killThread tid
