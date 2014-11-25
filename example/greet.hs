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
import Network.Wai
import Network.Wai.Handler.Warp

import Servant.API
import Servant.Client
import Servant.Docs
import Servant.Server

-- * Example

-- | A greet message data type
newtype Greet = Greet { msg :: Text }
  deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet

-- We add some useful annotations to our captures,
-- query parameters and request body to make the docs
-- really helpful.
instance ToCapture (Capture "name" Text) where
  toCapture _ = DocCapture "name" "name of the person to greet"

instance ToCapture (Capture "greetid" Text) where
  toCapture _ = DocCapture "greetid" "identifier of the greet msg to remove"

instance ToParam (QueryParam "capital" Bool) where
  toParam _ =
    DocQueryParam "capital"
                  ["true", "false"]
                  "Get the greeting message in uppercase (true) or not (false). Default is false."
                  Normal

instance ToSample Greet where
  toSample = Just $ Greet "Hello, haskeller!"

-- API specification
type TestApi =
       -- GET /hello/:name?capital={true, false}  returns a Greet as JSON
       "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get Greet

       -- POST /greet with a Greet as JSON in the request body,
       --             returns a Greet as JSON
  :<|> "greet" :> ReqBody Greet :> Post Greet

       -- DELETE /greet/:greetid
  :<|> "greet" :> Capture "greetid" Text :> Delete

testApi :: Proxy TestApi
testApi = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'EitherT (Int, String) IO' monad.
server :: Server TestApi
server = helloH :<|> postGreetH :<|> deleteGreetH

  where helloH name Nothing = helloH name (Just False)
        helloH name (Just False) = return . Greet $ "Hello, " <> name
        helloH name (Just True) = return . Greet . toUpper $ "Hello, " <> name

        postGreetH greet = return greet

        deleteGreetH _ = return ()

-- Client-side querying functions
--
-- They're all derived automatically from the type, and glued together
-- with :<|> just like in the type and for the server handlers, except
-- that we don't have to implement them!
clientApi :: Client TestApi
clientApi = client testApi

getGreet :: Text -> Maybe Bool -> BaseUrl -> EitherT String IO Greet
postGreet :: Greet -> BaseUrl -> EitherT String IO Greet
deleteGreet :: Text -> BaseUrl -> EitherT String IO ()
getGreet :<|> postGreet :<|> deleteGreet = clientApi

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
test :: Application
test = serve testApi server

-- Generate the data that lets us have API docs. This
-- is derived from the type as well as from
-- the 'ToCapture', 'ToParam' and 'ToSample' instances from above.
docsGreet :: API
docsGreet = docs testApi

-- Run the server.
--
-- 'run' comes from Network.Wai.Handler.Warp
runTestServer :: Port -> IO ()
runTestServer port = run port test

-- Put this all to work!
main :: IO ()
main = do
  -- we start the server, binding it to port 8001
  tid <- forkIO $ runTestServer 8001

  -- we tell the client where to find it
  let uri = BaseUrl Http "localhost" 8001

  -- we run a couple of requests against the server
  print =<< runEitherT (getGreet "alp" (Just True) uri)
  print =<< runEitherT (getGreet "alp" (Just False) uri)
  let g = Greet "yo"
  print =<< runEitherT (postGreet g uri)
  print =<< runEitherT (deleteGreet "blah" uri)
  killThread tid
  putStrLn "\n---------\n"

  -- we print the markdown docs
  putStrLn $ markdown docsGreet
