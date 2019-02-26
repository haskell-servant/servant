# How to get current route in servant-server?

This tries to be an answer to the [StackOverflow question](https://stackoverflow.com/questions/41538438/haskell-servant-get-current-route-url-from-handler).
There are two questions in one:

1. how to get current Request or URL?
2. how to get current "route"?

Note, that URL (e.g. `/route12/42`) is different than route
(e.g. `"route12" :> Capture "id" Int :> Get '[JSON] Int).
Let's see how we can solve both of these questions, right after a
short language pragma and import section.

```haskell
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans         #-}
module Main where

import Data.Maybe             (fromMaybe)
import Control.Monad.IO.Class (liftIO)
import System.Environment     (getArgs)
import GHC.Generics           (to, from, M1 (..), K1 (..), (:*:) (..))

-- for "unsafe" vault key creation
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString.Char8    as BS8
import qualified Data.Vault.Lazy          as V
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp

import Servant
import Servant.API.Generic 
import Servant.Server.Generic
import Servant.Server.Internal.RoutingApplication (passToServer)
```

## How to get current `Request` object or URL

Passing current `WAI` `Request` to the handler is actually quite easy.
This is "lazy" approach, we ask for "everything" in the request,
and we have to be careful in the handler (e.g. we cannot touch `requestBody`).
Also this "combinator" ties implementation to the `wai` server implementation,
which is an implementation detail
(nothing else in `servant-server` exposes `wai` internals, except of `Raw`).

The idea is to make `Server (Wai.Request :> api) = Wai.Request -> Server api`.
If we imagine for a second that we have such functionality in place,
we can write, using `Servant.API.Generic` (see "Using generics" cookbook recipe):

```haskell
data Routes1 route = Routes1
    { route11 :: route :- Wai.Request :> "route1" :> Get '[JSON] Int
    , route12 :: route :- Wai.Request :> "route2" :> Capture "id" Int :> Get '[JSON] Int
    }
  deriving (Generic)

routes1 :: Routes1 AsServer
routes1 = Routes1
    { route11 = \req -> liftIO $ do
        let p = Wai.rawPathInfo req
        BS8.putStrLn p
        return (BS8.length p)
    , route12 = \req i -> liftIO $ do
        let p = Wai.rawPathInfo req
        BS8.putStrLn p
        return (succ i)
    }

app1 :: Application
app1 = genericServe routes1
```

We define a `Routes1` data type, implement `Routes1 AsServer` value and turn it
into the `wai`'s `Application`. However, to compile this example, we need an
additional instance.  We use an *internal* `passToServer` combinator in the
implementation of `route`.

```haskell
instance HasServer api ctx => HasServer (Wai.Request :> api) ctx where
    type ServerT (Wai.Request :> api) m = Wai.Request -> ServerT api m
    
    hoistServerWithContext _ pc nt s =
        hoistServerWithContext (Proxy :: Proxy api) pc nt . s

    route _ ctx d = route (Proxy :: Proxy api) ctx $
        passToServer d id
```

This solution is good quick fix, but there are arguably better ways.

### Specific combinator

We may notice that both our handlers use `Wai.rawPathInto req` call.
That should alert us. Specific combinator is more elegant.
An ability to create new combinators outside the core framework,
is one of design principles of `servant`.

```haskell
data RawPathInfo

instance HasServer api ctx => HasServer (RawPathInfo :> api) ctx where
    type ServerT (RawPathInfo :> api) m = BS8.ByteString -> ServerT api m
    
    hoistServerWithContext _ pc nt s =
        hoistServerWithContext (Proxy :: Proxy api) pc nt . s

    route _ ctx d = route (Proxy :: Proxy api) ctx $
        passToServer d Wai.rawPathInfo
```

Using new `RawPathInfo` combinator, we can re-implement our application:

```haskell
data Routes2 route = Routes2
    { route21 :: route :- RawPathInfo :> "route1" :> Get '[JSON] Int
    , route22 :: route :- RawPathInfo :> "route2" :> Capture "id" Int :> Get '[JSON] Int
    }
  deriving (Generic)

routes2 :: Routes2 AsServer
routes2 = Routes2
    { route21 = \p -> liftIO $ do
        BS8.putStrLn p
        return (BS8.length p)
    , route22 = \p i -> liftIO $ do
        BS8.putStrLn p
        return (succ i)
    }

app2 :: Application
app2 = genericServe routes2
```

This version is slightly more declarative, and handlers are more restrictive.
We moved the `rawPathInfo` selector from handlers to combinator implementation,
removed repetition.

### Using `Vault`

The `vault` value in `wai` `Request` is not well known or used.
But in this scenario it can be useful.
Vault is explained in [Using WAI's vault for fun and profit](https://www.yesodweb.com/blog/2015/10/using-wais-vault) blog post.
It fills a "dynamic" gap of strongly typed `Request`: we can attach arbitrary data to the request,
as is common in web frameworks in a dynamically typed languages.
As `servant-server` is based on `wai`, using vault is the third answer
to the first part of the question.

We (unsafely) create a key to the vault:

```haskell
rpiKey :: V.Key BS8.ByteString
rpiKey = unsafePerformIO V.newKey
```

Then we create a *middleware* which will put `rawPathInfo` into the `vault`.

```haskell
middleware :: Wai.Middleware
middleware app req respond = do
    let vault' = V.insert rpiKey (Wai.rawPathInfo req) (Wai.vault req)
        req' = req { Wai.vault = vault' }
    app req' respond
```

Using this we make third variant of our application.
Note that we values might not be in the vault,
that's small functional regression.

```haskell
data Routes3 route = Routes3
    { route31 :: route :- Vault :> "route1" :> Get '[JSON] Int
    , route32 :: route :- Vault :> "route2" :> Capture "id" Int :> Get '[JSON] Int
    }
  deriving (Generic)

routes3 :: Routes3 AsServer
routes3 = Routes3
    { route31 = \v -> liftIO $ do
        let p = fromMaybe "?" $ V.lookup rpiKey v
        BS8.putStrLn p
        return (BS8.length p)
    , route32 = \v i -> liftIO $ do
        let p = fromMaybe "?" $ V.lookup rpiKey v
        BS8.putStrLn p
        return (succ i)
    }

app3 :: Application
app3 = middleware $ genericServe routes3
```

Note: that `vault` can be used to pass information from middlewares to handlers
and from handlers to middlewares. For example, the authentication can be done
completely in the middleware, with a user information stored in the vault for
handlers to use.

## How to get current *route*?

The second part of a question, is how to get current route.
Something, we can get `route2/:id` out?
Note that handlers are *anonymous*, in the same sense functions are.
E.g. to write *recursive* anonymous functions, we can use `fix` combinator.
We can use something close to that to pass "route into itself",
using `Servant.API.Generics` we can reduce the boilerplate too.

We start with ordinary looking `Routes4` data structure.

```haskell
data Routes4 route = Routes4
    { route41 :: route :- "route1" :> Get '[JSON] Int
    , route42 :: route :- "route2" :> Capture "id" Int :> Get '[JSON] Int
    }
  deriving (Generic)
```

But instead of making a `Routes4 AsServer` value, we'll use a different *mode*.
`AsRecServer route` is a handler which takes `route :- api` as a first
argument.  In this example we use `HasLink'`, but reader is free to use other
automatic interpretations, e.g. `servant-client` to make a proxy!

```haskell
data AsRecServer route
instance GenericMode (AsRecServer route) where
    type AsRecServer route :- api = (route :- api) -> (AsServer :- api)

routes4 :: Routes4 (AsRecServer (AsLink Link))
routes4 = Routes4
    { route41 = \l -> liftIO $ do
        print l
        return 42
    , route42 = \l i -> liftIO $ do
        print (l i)
        return i
    }

app4 :: Application
app4 = genericRecServe routes4
```

The usage is very simple, unfortunately the implementation is not.

### Hairy bits

The implementation of `genericRecServe` is intimidating.
The missing bit is a function `genericHoist`.
In short, given a function which can convert `modeA :- api` into `modeB :- api` for all `api`,
`genericHoist` converts `routes modeA` into `routes modeB`.
Maybe this function should exist in `Servant.API.Generic`?

```haskell
genericHoist
    :: ( GenericMode modeA, GenericMode modeB
       , Generic (routes modeA), Generic (routes modeB)
       , GServantHoist c api modeA modeB (Rep (routes modeA)) (Rep (routes modeB))
       )
    => Proxy modeA -> Proxy modeB -> Proxy c -> Proxy api
    -> (forall api'. c api' => Proxy api' -> (modeA :- api') -> (modeB :- api'))
    -> routes modeA -> routes modeB
genericHoist pa pb pc api nt = to . gservantHoist pa pb pc api nt . from
```

`genericRecServe` is `genericHoist` precomposed with a variant of `genericServe`.
The implementation of one-liner, given a wall of constraints.

```haskell
genericRecServe
    :: forall routes.
       ( HasServer (ToServantApi routes) '[]
       , GenericServant routes AsApi
       , GenericServant routes AsServer
       , GenericServant routes (AsRecServer (AsLink Link))
       , Server (ToServantApi routes) ~ ToServant routes AsServer
       , GServantHoist 
          HasLink'
          (ToServantApi routes)
          (AsRecServer (AsLink Link))
          AsServer
          (Rep (routes (AsRecServer (AsLink Link))))
          (Rep (routes AsServer))
       )
    => routes (AsRecServer (AsLink Link)) -> Application
genericRecServe
    = serve (Proxy :: Proxy (ToServantApi routes)) 
    . toServant
    . genericHoist
        (Proxy :: Proxy (AsRecServer (AsLink Link)))
        (Proxy :: Proxy AsServer)
        (Proxy :: Proxy HasLink')
        (genericApi (Proxy :: Proxy routes))
        (\p f -> f $ safeLink p p)
```

There we us single-instance-class trick to make partially applicable `HasLink`.

```haskell
class (IsElem api api, HasLink api) => HasLink' api
instance (IsElem api api, HasLink api) => HasLink' api
```

The work horse of `genericHoist` is `gservantHoist` which works
on `Rep` of route structures.
It's important to notice that `c` and `api` arguments are class arguments.
This let us constraint them in instances.

```haskell
class GServantHoist c api modeA modeB f g where
    gservantHoist
        :: Proxy modeA -> Proxy modeB -> Proxy c -> Proxy api
        -> (forall api'. c api' => Proxy api' -> (modeA :- api') -> (modeB :- api'))
        -> f x -> g x
```

Instance for `M1` (metadata) and `:*:` (product) are straight-forward
pass-through, something you would expect:

```haskell
instance
    GServantHoist c api modeA modeB f g
    =>
    GServantHoist c api modeA modeB (M1 i j f) (M1 i' j' g)
  where
    gservantHoist pa pb pc api nt
        = M1
        . gservantHoist pa pb pc api nt
        . unM1

instance
    ( GServantHoist c apiA modeA modeB f f'
    , GServantHoist c apiB modeA modeB g g'
    ) =>
    GServantHoist c (apiA :<|> apiB) modeA modeB (f :*: g) (f' :*: g')
  where
    gservantHoist pa pb pc _ nt (f :*: g) =
        gservantHoist pa pb pc (Proxy :: Proxy apiA) nt f 
        :*:
        gservantHoist pa pb pc (Proxy :: Proxy apiB) nt g
```

The implementation for the leaf `K1` shows why we need `c` and `api`
as class arguments: here we require `c api`, and "coherence" conditions,
so `api`, `modeA`, `modeB`, `x` and `y` match.

```haskell
instance
    ( c api, (modeA :- api) ~ x, (modeB :- api) ~ y )
    => GServantHoist c api modeA modeB (K1 i x) (K1 i y)
  where
    gservantHoist _pa _pb _pc api nt
        = K1
        . nt api
        . unK1
```

### Conclusion

Using similar `Generic` approach, we can do various transformations on handlers.
For example we can wrap ordinary routes in `servant` "middleware", which would
put route information into `vault`, and that information may be used by `wai`
`Middleware` to collect statistics. This way we can make an improved version of
`servant-ekg`, as currently `servant-ekg` may get confused by overlapping routes.

## Main for testing

```haskell
main :: IO ()
main = do
    args <- getArgs
    case args of
        ("run1":_) -> run app1
        ("run2":_) -> run app2
        ("run3":_) -> run app3
        ("run4":_) -> run app4
        _ -> putStrLn "To run, pass 'run1' argument: cabal new-run cookbook-generic run"
  where
    run app = do
        putStrLn "Starting cookbook-current-route at http://localhost:8000"
        Warp.run 8000 app
```
