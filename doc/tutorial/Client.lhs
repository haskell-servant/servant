# Querying an API

While defining handlers that serve an API has a lot to it, querying an API is simpler: we do not care about what happens inside the webserver, we just need to know how to talk to it and get a response back. Except that we usually have to write the querying functions by hand because the structure of the API isn't a first class citizen and can't be inspected to generate a bunch of client-side functions.

**servant** however has a way to inspect APIs, because APIs are just Haskell types and (GHC) Haskell lets us do quite a few things with types. In the same way that we look at an API type to deduce the types the handlers should have, we can inspect the structure of the API to *derive* Haskell functions that take one argument for each occurence of `Capture`, `ReqBody`, `QueryParam`
and friends. By *derive*, we mean that there's no code generation involved, the functions are defined just by the structure of the API type.

The source for this tutorial section is a literate haskell file, so first we
need to have some language extensions and imports:

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Client where

import Control.Monad.Trans.Except
import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import System.IO.Unsafe
```

Also, we need examples for some domain specific data types:

``` haskell
data Position = Position
  { x :: Int
  , y :: Int
  } deriving (Show, Generic)

instance FromJSON Position

newtype HelloMessage = HelloMessage { msg :: String }
  deriving (Show, Generic)

instance FromJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String
  , clientEmail :: String
  , clientAge :: Int
  , clientInterestedIn :: [String]
  } deriving Generic

instance ToJSON ClientInfo

data Email = Email
  { from :: String
  , to :: String
  , subject :: String
  , body :: String
  } deriving (Show, Generic)

instance FromJSON Email
```

Enough chitchat, let's see an example. Consider the following API type from the previous section:

``` haskell
type API = "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
      :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email
```

What we are going to get with **servant-client** here is 3 functions, one to query each endpoint:

``` haskell
position :: Int -- ^ value for "x"
         -> Int -- ^ value for "y"
         -> ExceptT ServantError IO Position

hello :: Maybe String -- ^ an optional value for "name"
      -> ExceptT ServantError IO HelloMessage

marketing :: ClientInfo -- ^ value for the request body
          -> ExceptT ServantError IO Email
```

Each function makes available as an argument any value that the response may
depend on, as evidenced in the API type. How do we get these functions? By calling
the function `client`. It takes three arguments:

- a `Proxy` to your API,
- a `BaseUrl`, consisting of the protocol, the host, the port and an optional subpath --
  this basically tells `client` where the service that you want to query is hosted,
- a `Manager`, (from [http-client](http://hackage.haskell.org/package/http-client))
which manages http connections.

``` haskell
api :: Proxy API
api = Proxy

{-# NOINLINE __manager #-}
__manager :: Manager
__manager = unsafePerformIO $ newManager defaultManagerSettings

position :<|> hello :<|> marketing =
  client api (BaseUrl Http "localhost" 8081 "") __manager
```

(Yes, the usage of `unsafePerformIO` is very ugly, we know. Hopefully soon it'll
be possible to do without.)

As you can see in the code above, we just "pattern match our way" to these functions. If we try to derive less or more functions than there are endpoints in the API, we obviously get an error. The `BaseUrl` value there is just:

``` haskell ignore
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

``` haskell
queries :: ExceptT ServantError IO (Position, HelloMessage, Email)
queries = do
  pos <- position 10 10
  message <- hello (Just "servant")
  em  <- marketing (ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"])
  return (pos, message, em)

run :: IO ()
run = do
  res <- runExceptT queries
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (pos, message, em) -> do
      print pos
      print message
      print em
```

Here's the output of the above code running against the appropriate server:

``` bash
Position {x = 10, y = 10}
HelloMessage {msg = "Hello, servant"}
Email {from = "great@company.com", to = "alp@foo.com", subject = "Hey Alp, we miss you!", body = "Hi Alp,\n\nSince you've recently turned 26, have you checked out our latest haskell, mathematics products? Give us a visit!"}
```

The types of the arguments for the functions are the same as for (server-side) request handlers. You now know how to use **servant-client**!
