# Querying an API

While defining handlers that [serve an API](Server.html) has a lot to it, querying an API is simpler: we do not care about what happens inside the webserver, we just need to know how to talk to it and get a response back. That said, we usually have to write the querying functions by hand because the structure of the API isn't a first class citizen and can't be inspected to generate the client-side functions.

**servant** however has a way to inspect APIs, because APIs are just Haskell types and (GHC) Haskell lets us do quite a few things with types. In the same way that we look at an API type to deduce the types the handlers should have, we can inspect the structure of the API to *derive* Haskell functions that take one argument for each occurrence of `Capture`, `ReqBody`, `QueryParam`
and friends (see [the tutorial introduction](ApiType.html) for an overview). By *derive*, we mean that there's no code generation involved - the functions are defined just by the structure of the API type.

The source for this tutorial section is a literate Haskell file, so first we need to have some language extensions and imports:

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Client where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import Servant.Types.SourceT (foreach)

import qualified Servant.Client.Streaming as S
```

Also, we need examples for some domain specific data types:

``` haskell
data Position = Position
  { xCoord :: Int
  , yCoord :: Int
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

What we are going to get with **servant-client** here is three functions, one to query each endpoint:

``` haskell
position :: Int -- ^ value for "x"
         -> Int -- ^ value for "y"
         -> ClientM Position

hello :: Maybe String -- ^ an optional value for "name"
      -> ClientM HelloMessage

marketing :: ClientInfo -- ^ value for the request body
          -> ClientM Email
```

Each function makes available as an argument any value that the response may
depend on, as evidenced in the API type. How do we get these functions? By calling
the function `client`. It takes one argument:

- a `Proxy` to your API,

``` haskell
api :: Proxy API
api = Proxy

position :<|> hello :<|> marketing = client api
```

`client api` returns client functions for our _entire_ API, combined with `:<|>`, which we can pattern match on as above. You could say `client` "calculates" the correct type and number of client functions for the API type it is given (via a `Proxy`), as well as their implementations.

If you have an `EmptyAPI` in your API, servant-client will hand you a value of
type `EmptyClient` in the corresponding slot, where `data EmptyClient =
EmptyClient`, as a way to indicate that you can't do anything useful with it.

``` haskell ignore
type API' = API :<|> EmptyAPI

api' :: Proxy API'
api' = Proxy

(position' :<|> hello' :<|> marketing') :<|> EmptyClient = client api'
```

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
  , baseUrlPath :: String   -- ^ path (eg "/a/b/c")
  }
```

That's it. Let's now write some code that uses our client functions.

``` haskell
queries :: ClientM (Position, HelloMessage, Email)
queries = do
  pos <- position 10 10
  message <- hello (Just "servant")
  em  <- marketing (ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"])
  pure (pos, message, em)

run :: IO ()
run = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM queries (mkClientEnv manager' (BaseUrl Http "localhost" 8081 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (pos, message, em) -> do
      print pos
      print message
      print em
```

Here's the output of the above code running against the appropriate server:

```
Position {xCoord = 10, yCoord = 10}
HelloMessage {msg = "Hello, servant"}
Email {from = "great@company.com", to = "alp@foo.com", subject = "Hey Alp, we miss you!", body = "Hi Alp,\n\nSince you've recently turned 26, have you checked out our latest haskell, mathematics products? Give us a visit!"}
```

The types of the arguments for the functions are the same as for (server-side) request handlers.

## Changing the monad the client functions live in

Just like `hoistServer` allows us to change the monad in which request handlers
of a web application live, we also have `hoistClient` for changing the monad
in which _client functions_ live. Consider the following trivial API:

``` haskell
type HoistClientAPI = Get '[JSON] Int :<|> Capture "n" Int :> Post '[JSON] Int

hoistClientAPI :: Proxy HoistClientAPI
hoistClientAPI = Proxy
```

We already know how to derive client functions for this API, and as we have
seen above they all return results in the `ClientM` monad when using `servant-client`.
However, `ClientM` is rarely (or never) the actual monad we need to use the client
functions in. Sometimes we need to run them in IO, sometimes in a custom monad
stack. `hoistClient` is a very simple solution to the problem of "changing" the monad
the clients run in.

``` haskell ignore
hoistClient
  :: HasClient ClientM api   -- we need a valid API
  => Proxy api               -- a Proxy to the API type
  -> (forall a. m a -> n a)  -- a "monad conversion function" (natural transformation)
  -> Client m api            -- clients in the source monad
  -> Client n api            -- result: clients in the target monad
```

The "conversion function" argument above, just like the ones given to `hoistServer`, must
be able to turn an `m a` into an `n a` for any choice of type `a`.

Let's see this in action on our example. We first derive our client functions as usual,
with all of them returning a result in `ClientM`.

``` haskell
getIntClientM :: ClientM Int
postIntClientM :: Int -> ClientM Int
getIntClientM :<|> postIntClientM = client hoistClientAPI
```

And we finally decide that we want the handlers to run in IO instead, by
"post-applying" `runClientM` to a fixed client environment.

``` haskell
-- our conversion function has type: forall a. ClientM a -> IO a
-- the result has type:
-- Client IO HoistClientAPI = IO Int :<|> (Int -> IO Int)
getClients :: ClientEnv -> Client IO HoistClientAPI
getClients clientEnv
  = hoistClient hoistClientAPI
                ( fmap (either (error . show) id)
                . flip runClientM clientEnv
                )
                (client hoistClientAPI)
```

## Querying Streaming APIs.

Consider the following streaming API type:

``` haskell
type StreamAPI = "positionStream" :> StreamGet NewlineFraming JSON (SourceIO Position)
```

Note that we use the same `SourceIO` type as on the server-side
(this is different from `servant-0.14`).
However, we have to use different client, `Servant.Client.Streaming`,
which can stream (but has different API).

In any case, here's how we write a function to query our API:

```haskell
streamAPI :: Proxy StreamAPI
streamAPI = Proxy

posStream :: S.ClientM (SourceIO Position)
posStream = S.client streamAPI
```

We'll get back a `SourceIO Position`. Instead of `runClientM`, the streaming
client provides `withClientM`: the underlying HTTP connection is open only
until the inner functions exits.  Inside the block we can e.g just print out
all elements from a `SourceIO`, to give some idea of how to work with them.

``` haskell
printSourceIO :: Show a => ClientEnv -> S.ClientM (SourceIO a) -> IO ()
printSourceIO env c = S.withClientM c env $ \e -> case e of
    Left err -> putStrLn $ "Error: " ++ show err
    Right rs -> foreach fail print rs
```

The stream is parsed and provided incrementally. So the above loop prints out
each result as soon as it is received on the stream, rather than waiting until
they are all available to print them at once.

You now know how to use **servant-client**!
