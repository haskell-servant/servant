# Record-based APIs: the simple case

This cookbook explains how to implement an API with a simple record-based
structure. We only deal with non-nested APIs in which every endpoint is on the same
level.

If a you need nesting because you have different branches in your API tree, you
might want to jump directly to the [Record-based APIs: the nested records
case](../namedRoutes/NamedRoutes.html) cookbook that broaches the subject.

Shall we begin?

## Why would I want to use `Records` over the alternative `:<|>` operator?

With a record-based API, we don’t need to care about the declaration order of the endpoints.
For example, with the `:<|>` operator there’s room for error when the order of the API type

```haskell,ignore
type API1  =   "version" :> Get '[JSON] Version
                :<|>  "movies" :> Get '[JSON] [Movie]
```
does not follow the `Handler` implementation order
```haskell,ignore
apiHandler :: ServerT API1 Handler
apiHandler =   getMovies
                 :<|> getVersion
```
GHC could scold you with a very tedious message such as :
```console
    • Couldn't match type 'Handler NoContent'
                     with 'Movie -> Handler NoContent'
      Expected type: ServerT MovieCatalogAPI Handler
        Actual type: Handler Version
                     :<|> ((Maybe SortBy -> Handler [Movie])
                           :<|> ((MovieId -> Handler (Maybe Movie))
                                 :<|> ((MovieId -> Movie -> Handler NoContent)
                                       :<|> (MovieId -> Handler NoContent))))
    • In the expression:
        versionHandler
          :<|>
            movieListHandler
              :<|>
                getMovieHandler :<|> updateMovieHandler :<|> deleteMovieHandler
      In an equation for 'server':
          server
            = versionHandler
                :<|>
                  movieListHandler
                    :<|>
                      getMovieHandler :<|> updateMovieHandler :<|> deleteMovieHandler
    |
226 | server = versionHandler
```
On the contrary, with the record-based technique, we refer to the routes by their name:
```haskell,ignore
data API mode = API
    { list   :: "list" :> ...
    , delete ::  "delete" :> ...
    }
```
and GHC follows the lead :
```console
    • Couldn't match type 'NoContent' with 'Movie'
      Expected type: AsServerT Handler :- Delete '[JSON] Movie
        Actual type: Handler NoContent
    • In the 'delete' field of a record
      In the expression:
        MovieAPI
          {get = getMovieHandler movieId,
           update = updateMovieHandler movieId,
           delete = deleteMovieHandler movieId}
      In an equation for 'movieHandler':
          movieHandler movieId
            = MovieAPI
                {get = getMovieHandler movieId,
                 update = updateMovieHandler movieId,
                 delete = deleteMovieHandler movieId}
    |
252 |     , delete = deleteMovieHandler movieId
```

So, records are more readable for a human, and GHC gives you more accurate error messages.

What are we waiting for?

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
First you define a record with field types prefixed by a parameter `mode`:

```haskell
data Routes mode = Routes
    { _get :: mode :- Capture "id" Int :> Get '[JSON] String
    , _put :: mode :- ReqBody '[JSON] Int :> Put '[JSON] Bool
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
        _ -> putStrLn "To run, pass 'run' argument: cabal run cookbook-generic run"
```

## Using record-based APIs together with a custom monad

If your app uses a custom monad, here's how you can combine it with
generics.

```haskell
data AppCustomState =
  AppCustomState

type AppM = ReaderT AppCustomState Handler

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
```
