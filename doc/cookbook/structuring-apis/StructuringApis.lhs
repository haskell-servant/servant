# Structuring APIs

In this recipe, we will see a few simple ways to
structure your APIs by splitting them up into smaller
"sub-APIs" or by sharing common structure between
different parts. Let's start with the usual throat
clearing.

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
import Data.Aeson
import GHC.Generics
import GHC.TypeLits
import Network.Wai.Handler.Warp
import Servant
```

Our application will consist of three different
"sub-APIs", with a few endpoints in each of them.
Our global API is defined as follows.

``` haskell
type API = FactoringAPI
      :<|> SimpleAPI "users" User UserId
      :<|> SimpleAPI "products" Product ProductId
```

We simply join the three different parts with `:<|>`,
as if each sub-API was just a simple endpoint.
The first part, `FactoringAPI`, shows how we can
"factor out" combinators that are common to several
endpoints, just like we turn `a * b + a * c` into
`a * (b + c)` in algebra.

``` haskell
-- Two endpoints:
--   - GET /x/<some 'Int'>[?y=<some 'Int'>]
--   - POST /x/<some 'Int'>
type FactoringAPI =
  "x" :> Capture "x" Int :>
      (    QueryParam "y" Int :> Get '[JSON] Int
      :<|>                       Post '[JSON] Int
      )

{- this is equivalent to:

type FactoringAPI' =
  "x" :> Capture "x" Int :> QueryParam "y" Int :> Get '[JSON] Int :<|>
  "x" :> Capture "x" Int :> Post '[JSON] Int
-}
```

You can see that both endpoints start with a static
path fragment, `/"x"`, then capture some arbitrary
`Int` until they finally differ. Now, this also has
an effect on the server for such an API, and its type
in particular. While the server for `FactoringAPI'` would
be made of a function of type `Int -> Maybe Int -> Handler Int`
and a function of type `Int -> Handler Int` glued with `:<|>`,
a server for `FactoringAPI` (without the `'`) reflects the
"factorisation" and therefore, `Server FactoringAPI` is
`Int -> (Maybe Int -> Handler Int :<|> Handler Int)`. That is, the
server must be a function that takes an `Int` (the `Capture`) and
returns two values glued with `:<|>`, one of type `Maybe Int -> Handler Int`
and the other of type `Handler Int`. Let's provide such a server
implementation, with those "nested types".

**Tip**: you can load this module in ghci and ask for the concrete
type that `Server FactoringAPI` "resolves to" by typing
`:kind! Server FactoringAPI`.

``` haskell
factoringServer :: Server FactoringAPI
factoringServer x = getXY :<|> postX

  where getXY Nothing  = return x
        getXY (Just y) = return (x + y)

        postX = return (x - 1)
```

If you want to avoid the "nested types" and the need to manually
dispatch the arguments (like `x` above) to the different request
handlers, and would just like to be able to declare the API type
as above but pretending that the `Capture` is not factored out,
that every combinator is "distributed" (i.e that all endpoints
are specified like `FactoringAPI'` above), then you should
look at `flatten` from the
[servant-flatten](https://hackage.haskell.org/package/servant-flatten)
package.

Next come the two sub-APIs defined in terms of this `SimpleAPI`
type, but with different parameters. That type is just a good old
Haskell type synonym that abstracts away a pretty common structure in
web services, where you have:

  - one endpoint for listing a bunch of entities of some type
  - one endpoint for accessing the entity with a given identifier
  - one endpoint for creating a new entity

There are many variants on this theme (endpoints for deleting,
paginated listings, etc). The simple definition below reproduces
such a structure, but instead of picking concrete types for
the entities and their identifiers, we simply let the user
of the type decide, by making those types parameters of
`SimpleAPI`. While we're at it, we'll put all our endpoints
under a common prefix that we also take as a parameter.

``` haskell
-- Three endpoints:
--   - GET /<name>
--   - GET /<name>/<some 'i'>
--   - POST /<name>
type SimpleAPI (name :: Symbol) a i = name :>
  (                         Get '[JSON] [a]
  :<|> Capture "id" i    :> Get '[JSON] a
  :<|> ReqBody '[JSON] a :> Post '[JSON] NoContent
  )
```

`Symbol` is the [kind](https://wiki.haskell.org/Kind)
of type-level strings, which is what servant uses for
representing static path fragments. We can even provide
a little helper function for creating a server for that API
given one handler for each endpoint as arguments.

``` haskell
simpleServer
  :: Handler [a]
  -> (i -> Handler a)
  -> (a -> Handler NoContent)
  -> Server (SimpleAPI name a i)
simpleServer listAs getA postA =
  listAs :<|> getA :<|> postA

{- you could alternatively provide such a definition
   but with the handlers running in another monad,
   or even an arbitrary one!

simpleAPIServer
  :: m [a]
  -> (i -> m a)
  -> (a -> m NoContent)
  -> ServerT (SimpleAPI name a i) m
simpleAPIServer listAs getA postA =
  listAs :<|> getA :<|> postA

   and use 'hoistServer' on the result of `simpleAPIServer`
   applied to your handlers right before you call `serve`.
-}
```

We can use this to define servers for the user and product
related sections of the API.

``` haskell
userServer :: Server (SimpleAPI "users" User UserId)
userServer = simpleServer
  (return [])
  (\userid -> return $
      if userid == 0
      then User "john" 64
      else User "everybody else" 10
  )
  (\_user -> return NoContent)

productServer :: Server (SimpleAPI "products" Product ProductId)
productServer = simpleServer
  (return [])
  (\_productid -> return $ Product "Great stuff")
  (\_product -> return NoContent)
```

Finally, some dummy types and the serving part.

``` haskell
type UserId = Int

data User = User { username :: String, age :: Int }
  deriving Generic

instance FromJSON User
instance ToJSON   User

type ProductId = Int

data Product = Product { productname :: String }
  deriving Generic

instance FromJSON Product
instance ToJSON   Product

api :: Proxy API
api = Proxy

main :: IO ()
main = run 8080 . serve api $
  factoringServer :<|> userServer :<|> productServer
```

This program is available as a cabal project
[here](https://github.com/haskell-servant/servant/tree/master/doc/cookbook/structuring-apis).
