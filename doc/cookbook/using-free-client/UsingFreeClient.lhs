# Inspecting, debugging, simulating clients and more

or simply put: _a practical introduction to `Servant.Client.Free`_.

Someone asked on IRC how one could access the intermediate Requests (resp. Responses)
produced (resp. received) by client functions derived using servant-client.
My response to such inquiries is: to extend `servant-client` in an ad-hoc way (e.g for testing or debugging
purposes), use `Servant.Client.Free`. This recipe shows how.

First the module header, but this time We'll comment the imports.

```haskell
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where
```

We will primarily use `Servant.Client.Free`, it doesn't re-export anything
from `free` package, so we need to import it as well.

```haskell
import Control.Monad.Free
import Servant.Client.Free
```

Also we'll use `servant-client` internals, which uses `http-client`,
so let's import them *qualified*

```haskell
import qualified Servant.Client.Internal.HttpClient as I
import qualified Network.HTTP.Client                as HTTP
```

The rest of the imports are for a server we implement here for completeness.

```haskell
import Servant
import Network.Wai.Handler.Warp (run)
import System.Environment       (getArgs)
```

## API & Main

We'll work with a very simple API:

```haskell
type API = "square" :> Capture "n" Int :> Get '[JSON] Int

api :: Proxy API
api = Proxy
```

Next we implement a `main`. If passed `"server"` it will run `server`, if passed
`"client"` it will run a `test` function (to be defined next). This should be
pretty straightforward:

```haskell
main :: IO ()
main = do
    args <- getArgs
    case args of
        ("server":_) -> do
            putStrLn "Starting cookbook-using-free-client at http://localhost:8000"
            run 8000 $ serve api $ \n -> pure (n * n)
        ("client":_) ->
            test
        _ -> do
            putStrLn "Try:"
            putStrLn "cabal run cookbook-using-free-client server"
            putStrLn "cabal run cookbook-using-free-client client"
```

## Test

In the client part, we will use a `Servant.Client.Free` client.
Because we have a single endpoint API, we'll get a single client function,
running in the `Free ClientF` (free) monad:

```haskell
getSquare :: Int -> Free ClientF Int
getSquare = client api
```

Such clients are "client functions without a backend", so to speak,
or where the backend has been abstracted out. To be more precise, `ClientF` is a functor that
precisely represents the operations servant-client-core needs from an http client backend.
So if we are to emulate one or augment what such a backend does, it will be by interpreting
all those operations, the way we want to. This also means we get access to the requests and
responses and can do anything we want with them, right when they are produced or consumed,
respectively.

Next, we can write our small test. We'll pass a value to `getSquare` and inspect
the `Free` structure. The first three possibilities are self-explanatory:

```haskell
test :: IO ()
test = case getSquare 42 of
    Pure n ->
        putStrLn $ "ERROR: got pure result: " ++ show n
    Free (Throw err) ->
        putStrLn $ "ERROR: got error right away: " ++ show err
```

We are interested in `RunRequest`, that's what client should block on:

```haskell
    Free (RunRequest req k) -> do
```

Then we need to prepare the context, get HTTP (connection) `Manager`
and `BaseUrl`:

```haskell
        burl <- parseBaseUrl "http://localhost:8000"
        mgr <- HTTP.newManager HTTP.defaultManagerSettings
```

Now we can use `servant-client`'s internals to convert servant's `Request`
to http-client's `Request`, and we can inspect it:

```haskell
        req' <- I.defaultMakeClientRequest burl req
        putStrLn $ "Making request: " ++ show req'
```

`servant-client`'s request does a little more, but this is good enough for
our demo. We get back an http-client `Response` which we can also inspect.

```haskell
        res' <- HTTP.httpLbs req' mgr
        putStrLn $ "Got response: " ++ show res'
```

And we continue by turning http-client's `Response` into servant's `Response`,
and calling the continuation. We should get a `Pure` value.

```haskell
        let res = I.clientResponseToResponse id res'

        case k res of
            Pure n ->
                putStrLn $ "Expected 1764, got " ++ show n
            _ ->
                putStrLn "ERROR: didn't get a response"
```

So that's it. Using `Free` we can evaluate servant clients step-by-step, and
validate that the client functions or the HTTP client backend does what we expect
(e.g by printing requests/responses on the fly). In fact, using `Servant.Client.Free`
is a little simpler than defining a custom `RunClient` instance, especially
for those cases where it is handy to have the full sequence of client calls
and responses available for us to inspect, since `RunClient` only gives us
access to one `Request` or `Response` at a time.

On the other hand, a "batch collection" of requests and/or responses can be achieved
with both free clients and a custom `RunClient` instance rather easily, for example
by using a `Writer [(Request, Response)]` monad.

Here is an example of running our small `test` against a running server:

```
Making request: Request {
  host                 = "localhost"
  port                 = 8000
  secure               = False
  requestHeaders       = [("Accept","application/json;charset=utf-8,application/json")]
  path                 = "/square/42"
  queryString          = ""
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
}

Got response: Response
  { responseStatus = Status {statusCode = 200, statusMessage = "OK"}
  , responseVersion = HTTP/1.1
  , responseHeaders =
    [ ("Transfer-Encoding","chunked")
    , ("Date","Thu, 05 Jul 2018 21:12:41 GMT")
    , ("Server","Warp/3.2.22")
    , ("Content-Type","application/json;charset=utf-8")
    ]
  , responseBody = "1764"
  , responseCookieJar = CJ {expose = []}
  , responseClose' = ResponseClose
  }

Expected 1764, got 1764
```
