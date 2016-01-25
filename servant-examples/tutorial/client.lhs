---
title: Deriving Haskell functions to query an API
toc: true
---

While defining handlers that serve an API has a lot to it, querying an API is simpler: we do not care about what happens inside the webserver, we just need to know how to talk to it and get a response back. Except that we usually have to write the querying functions by hand because the structure of the API isn't a first class citizen and can't be inspected to generate a bunch of client-side functions.

*servant* however has a way to inspect API, because APIs are just Haskell types and (GHC) Haskell lets us do quite a few things with types. In the same way that we look at an API type to deduce the types the handlers should have, we can inspect the structure of the API to *derive* Haskell functions that take one argument for each occurence of `Capture`, `ReqBody`, `QueryParam`
and friends. By *derive*, we mean that there's no code generation involved, the functions are defined just by the structure of the API type.

The source for this tutorial section is a literate haskell file, so first we
need to have some language extensions and imports:

> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE DeriveGeneric #-}
> {-# LANGUAGE TypeOperators #-}
>
> module Client where
>
> import Control.Monad.Trans.Either
> import Data.Aeson
> import Data.Proxy
> import GHC.Generics
> import Servant.API
> import Servant.Client

Also, we need examples for some domain specific data types:

> data Position = Position
>   { x :: Int
>   , y :: Int
>   } deriving (Show, Generic)
>
> instance FromJSON Position
>
> newtype HelloMessage = HelloMessage { msg :: String }
>   deriving (Show, Generic)
>
> instance FromJSON HelloMessage
>
> data ClientInfo = ClientInfo
>   { clientName :: String
>   , clientEmail :: String
>   , clientAge :: Int
>   , clientInterestedIn :: [String]
>   } deriving Generic
>
> instance ToJSON ClientInfo
>
> data Email = Email
>   { from :: String
>   , to :: String
>   , subject :: String
>   , body :: String
>   } deriving (Show, Generic)
>
> instance FromJSON Email

Enough chitchat, let's see an example. Consider the following API type from the previous section:

> type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
>       :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
>       :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

What we are going to get with *servant-client* here is 3 functions, one to query each endpoint:

> position :: Int -- ^ value for "x"
>          -> Int -- ^ value for "y"
>          -> EitherT ServantError IO Position
>
> hello :: Maybe String -- ^ an optional value for "name"
>       -> EitherT ServantError IO HelloMessage
>
> marketing :: ClientInfo -- ^ value for the request body
>           -> EitherT ServantError IO Email

Each function makes available as an argument any value that the response may depend on, as evidenced in the API type. How do we get these functions? Just give a `Proxy` to your API and a host to make the requests to:

> api :: Proxy API
> api = Proxy
>
> position :<|> hello :<|> marketing = client api (BaseUrl Http "localhost" 8081)

As you can see in the code above, we just "pattern match our way" to these functions. If we try to derive less or more functions than there are endpoints in the API, we obviously get an error. The `BaseUrl` value there is just:

``` haskell
-- | URI scheme to use
data Scheme =
    Http  -- ^ http://
  | Https -- ^ https://
  deriving

-- | Simple data type to represent the target of HTTP requests
--   for servant's automatically-generated clients.
data BaseUrl = BaseUrl
  { baseUrlScheme :: Scheme -- ^ URI scheme to use
  , baseUrlHost :: String   -- ^ host (eg "haskell.org")
  , baseUrlPort :: Int      -- ^ port (eg 80)
  }
```

That's it. Let's now write some code that uses our client functions.

> queries :: EitherT ServantError IO (Position, HelloMessage, Email)
> queries = do
>   pos <- position 10 10
>   msg <- hello (Just "servant")
>   em  <- marketing (ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"])
>   return (pos, msg, em)
>
> run :: IO ()
> run = do
>   res <- runEitherT queries
>   case res of
>     Left err -> putStrLn $ "Error: " ++ show err
>     Right (pos, msg, em) -> do
>       print pos
>       print msg
>       print em

You can now run `dist/build/tutorial/tutorial 8` (the server) and
`dist/build/t8-main/t8-main` (the client) to see them both in action.

``` bash
 $ dist/build/tutorial/tutorial 8
 # and in another terminal:
 $ dist/build/t8-main/t8-main
 Position {x = 10, y = 10}
 HelloMessage {msg = "Hello, servant"}
 Email {from = "great@company.com", to = "alp@foo.com", subject = "Hey Alp, we miss you!", body = "Hi Alp,\n\nSince you've recently turned 26, have you checked out our latest haskell, mathematics products? Give us a visit!"}
```

The types of the arguments for the functions are the same as for (server-side) request handlers. You now know how to use *servant-client*!

<div style="text-align: center;">
  <p><a href="/tutorial/server.html">Previous page: Serving an API</a></p>
  <p><a href="/tutorial/javascript.html">Next page: Generating javascript functions to query an API</a></p>
</div>
