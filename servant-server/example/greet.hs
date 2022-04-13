{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

import           Prelude ()
import           Prelude.Compat

import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp

import           Servant
import           Servant.Server.Generic ()

-- * Example

-- | A greet message data type
newtype Greet = Greet { _msg :: Text }
  deriving (Generic, Show)

instance FromJSON Greet
instance ToJSON Greet

-- API specification
type TestApi' =
       -- GET /hello/:name?capital={true, false}  returns a Greet as JSON
       "hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON] Greet

       -- POST /greet with a Greet as JSON in the request body,
       --             returns a Greet as JSON
  :<|> "greet" :> ReqBody '[JSON] Greet :> Post '[JSON] Greet

       -- DELETE /greet/:greetid
  :<|> "greet" :> Capture "greetid" Text :> Delete '[JSON] NoContent

  :<|> NamedRoutes OtherRoutes

type TestApi =
       TestApi'
  :<|> "redirect" :> Capture "redirectValue" Int :> RedirectOf TestApi'


data OtherRoutes mode = OtherRoutes
  { version :: mode :- Get '[JSON] Int
  , bye :: mode :- "bye" :> Capture "name" Text :> Get '[JSON] Text
  }
  deriving Generic

testApi :: Proxy TestApi
testApi = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'Handler' monad.
server :: Server TestApi
server = (helloH :<|> postGreetH :<|> deleteGreetH :<|> otherRoutes) :<|> redirect
  where otherRoutes = OtherRoutes {..}

        bye name = pure $ "Bye, " <> name <> " !"
        version = pure 42

        helloH name Nothing = helloH name (Just False)
        helloH name (Just False) = return . Greet $ "Hello, " <> name
        helloH name (Just True) = return . Greet . toUpper $ "Hello, " <> name

        postGreetH greet = return greet

        deleteGreetH _ = return NoContent

        redirect 42 = pure $
          RedirectOf (Proxy @("hello" :> Capture "name" Text :> QueryParam "capital" Bool :> Get '[JSON] Greet))
          (\buildPath -> buildPath "Nicolas" (Just True))

        redirect _ = pure $
          RedirectOf (namedRoute @"bye" @OtherRoutes)
          (\buildPath -> buildPath "Gaël")

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
test :: Application
test = serve testApi server

-- Run the server.
--
-- 'run' comes from Network.Wai.Handler.Warp
runTestServer :: Port -> IO ()
runTestServer port = run port test

-- Put this all to work!
main :: IO ()
main = runTestServer 8001
