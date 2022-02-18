# Using generics

```haskell
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}
module Main (main, api, getLink, routesLinks, cliGet) where

import Control.Exception          (throwIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Proxy                 (Proxy (..))
import Network.Wai.Handler.Warp   (run)
import System.Environment         (getArgs)

import Servant
import Servant.Client

import Servant.API.Generic
import Servant.Client.Generic
import Servant.Server.Generic
```

The usage is simple, if you only need a collection of routes.
First you define a record with field types prefixed by a parameter `route`:

```haskell
data Routes route = Routes
    { _get :: route :- Capture "id" Int :> Get '[JSON] String
    , _put :: route :- ReqBody '[JSON] Int :> Put '[JSON] Bool
    }
  deriving (Generic)
```

Then we'll use this data type to define API, links, server and client.

## API

You can get a `Proxy` of the API using `genericApi`:

```haskell
api :: Proxy (ToServantApi Routes)
api = genericApi (Proxy :: Proxy Routes)
```

It's recommended to use `genericApi` function, as then you'll get
better error message, for example if you forget to `derive Generic`.

## Links

The clear advantage of record-based generics approach, is that
we can get safe links very conveniently. We don't need to define endpoint types,
as field accessors work as proxies:

```haskell
getLink :: Int -> Link
getLink = fieldLink _get
```

We can also get all links at once, as a record:

```haskell
routesLinks :: Routes (AsLink Link)
routesLinks = allFieldLinks
```

## Client

Even more power starts to show when we generate a record of client functions.
Here we use `genericClientHoist` function, which lets us simultaneously
hoist the monad, in this case from `ClientM` to `IO`.

```haskell
cliRoutes :: Routes (AsClientT IO)
cliRoutes = genericClientHoist
    (\x -> runClientM x env >>= either throwIO return)
  where
    env = error "undefined environment"

cliGet :: Int -> IO String
cliGet = _get cliRoutes
```

## Server

Finally, probably the most handy usage: we can convert record of handlers into
the server implementation:

```haskell
record :: Routes AsServer
record = Routes
    { _get = return . show
    , _put = return . odd
    }

app :: Application
app = genericServe record

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("run":_) -> do
            putStrLn "Starting cookbook-generic at http://localhost:8000"
            run 8000 app
        -- see this cookbook below for custom-monad explanation
        ("run-custom-monad":_) -> do
            putStrLn "Starting cookbook-generic with a custom monad at http://localhost:8000"
            run 8000 (appMyMonad AppCustomState)
        _ -> putStrLn "To run, pass 'run' argument: cabal new-run cookbook-generic run"
```

## Using generics together with a custom monad

If your app uses a custom monad, here's how you can combine it with
generics.

```haskell
data AppCustomState =
  AppCustomState

type AppM = ReaderT AppCustomState Handler

apiMyMonad :: Proxy (ToServantApi Routes)
apiMyMonad = genericApi (Proxy :: Proxy Routes)

getRouteMyMonad :: Int -> AppM String
getRouteMyMonad = return . show

putRouteMyMonad :: Int -> AppM Bool
putRouteMyMonad = return . odd

recordMyMonad :: Routes (AsServerT AppM)
recordMyMonad = Routes {_get = getRouteMyMonad, _put = putRouteMyMonad}

-- natural transformation
nt :: AppCustomState -> AppM a -> Handler a
nt s x = runReaderT x s

appMyMonad :: AppCustomState -> Application
appMyMonad state = genericServeT (nt state) recordMyMonad
