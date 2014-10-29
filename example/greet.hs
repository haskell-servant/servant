{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Concurrent (forkIO, killThread)
import Control.Monad.Trans.Either
import Data.Aeson
import Data.Monoid
import Data.Proxy
import Data.Text
import GHC.Generics
import Network.URI
import Network.Wai
import Network.Wai.Handler.Warp

import Servant.API
import Servant.Client
import Servant.Docs
import Servant.Server

-- * Example

data Greet = Greet { _msg :: Text }
  deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet

instance ToCapture (Capture "name" Text) where
  toCapture _ = DocCapture "name" "name of the person to greet"

instance ToCapture (Capture "greetid" Text) where
  toCapture _ = DocCapture "greetid" "identifier of the greet msg to remove"

instance ToParam (QueryParam "capital" Bool) where
  toParam _ =
    DocQueryParam "capital"
                  ["true", "false"]
                  "Get the greeting message in uppercase (true) or not (false). Default is false."

instance ToSample Greet where
  toSample Proxy = Just (encode g)

    where g = Greet "Hello, haskeller!"

-- API specification
type TestApi =
       "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get Greet
  :<|> "greet" :> ReqBody Greet :> Post Greet
  :<|> "delete" :> Capture "greetid" Text :> Delete

type TestApi2 = [sitemap|
GET      hello/name:Text/?capital:Bool   () -> Greet
POST     greet                           Greet -> Greet
DELETE   delete/greetid:Text             ()
|]

testApi :: Proxy TestApi
testApi = Proxy

-- Server-side handlers
server :: Server TestApi
server = hello :<|> greet :<|> delete

  where hello name Nothing = hello name (Just False)
        hello name (Just False) = return . Greet $ "Hello, " <> name
        hello name (Just True) = return . Greet . toUpper $ "Hello, " <> name

        greet = return

        delete _ = return ()

-- Client-side query functions
clientApi :: Client TestApi
clientApi = client testApi

getGreet :: Text -> Maybe Bool -> URI -> EitherT String IO Greet
postGreet :: Greet -> URI -> EitherT String IO Greet
deleteGreet :: Text -> URI -> EitherT String IO ()
getGreet :<|> postGreet :<|> deleteGreet = clientApi

-- Turn the server into a WAI app
test :: Application
test = serve testApi server

-- Documentation
docsGreet :: API
docsGreet = docs testApi

-- Run the server
runTestServer :: Port -> IO ()
runTestServer port = run port test

-- Run some queries against the server
main :: IO ()
main = do
  tid <- forkIO $ runTestServer 8001
  let Just uri = parseURI "http://localhost:8001"
  print =<< runEitherT (getGreet "alp" (Just True) uri)
  print =<< runEitherT (getGreet "alp" (Just False) uri)
  let g = Greet "yo"
  print =<< runEitherT (postGreet g uri)
  print =<< runEitherT (deleteGreet "blah" uri)
  killThread tid
  putStrLn "\n---------\n"
  printMarkdown docsGreet
