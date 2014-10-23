{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant where

import Control.Applicative
import Data.Aeson
import Data.Monoid
import Data.Proxy
import Data.Text
import GHC.Generics
import GHC.TypeLits
import Network.Wai
import Network.Wai.Handler.Warp
import Soenke

-- * Captures
data Capture sym a

class FromText a where
  capture :: Text -> Maybe a

instance FromText Text where
  capture = Just

captured :: FromText a => proxy (Capture sym a) -> Text -> Maybe a
captured _ = capture

instance (KnownSymbol capture, FromText a, HasServer sublayout)
      => HasServer (Capture capture a :> sublayout) where

  -- this means that what follows the capture
  -- must necessarily be a function that expects
  -- the captured value. :-(
  type Server (Capture capture a :> sublayout) =
    Capture capture a :> (a -> Server sublayout)

  route Proxy (capture :> subserver) request = case pathInfo request of
    (first : rest)
      -> case captured capture first of
           Nothing  -> return Nothing
           Just v   -> route (Proxy :: Proxy sublayout) (subserver v) request{
                         pathInfo = rest
                       }
    _ -> return Nothing

-- * Request Body support
data RQBody a

instance (FromJSON a, HasServer sublayout)
      => HasServer (RQBody a :> sublayout) where

  -- same caveat as for the Captures...
  type Server (RQBody a :> sublayout) =
    RQBody a :> (a -> Server sublayout)

  route Proxy (rqbody :> subserver) request = do
    mrqbody <- decode' <$> lazyRequestBody request
    case mrqbody of
      Nothing -> return Nothing
      Just v  -> route (Proxy :: Proxy sublayout) (subserver v) request

-- * Example

type TestApi = "hello" :> Capture "name" Text :> Get Greet

testApi :: Proxy TestApi
testApi = Proxy

data Greet = Greet { msg :: Text }
  deriving Generic

instance ToJSON Greet

server :: Server TestApi
server =
     Proxy -- :: Proxy "hello"
  :> Proxy -- :: Proxy (Capture "name" Text)
  :> (return . func)

  where func name = Greet ("Hello, " <> name)

test :: Application
test = serve testApi server

runTest :: Port -> IO ()
runTest port = run port test

-- load in ghci, call 'runTest 8000'
-- visit http://localhost:8000/hello/world
-- visit http://localhost:8000/hello/soenke
-- visit http://localhost:8000/hello/alp
