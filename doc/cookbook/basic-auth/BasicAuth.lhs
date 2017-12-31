# Basic Authentication

Let's see a simple example of a web application with a
single endpoint, protected by
[Basic Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication).

First, some throat clearing.

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
import Control.Concurrent
import Control.Exception
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
```

We will be dealing with a very simple model of users, as shown below.
Our "user database" will just be a map from usernames to full user details.
For the sake of simplicity, it will just be read only but the same code could
be used with mutable references, database connections, files and more in place
of our `Map`.

``` haskell
type Username = T.Text
type Password = T.Text
type Website = T.Text

data User = User
  { user :: Username
  , pass :: Password
  , site :: Website
  } deriving (Eq, Show)

-- could be a postgres connection, a file, anything.
type UserDB = Map.Map Username User

-- create a "database" from a list of users
createUserDB :: [User] -> UserDB
createUserDB users = Map.fromList [ (user u, u) | u <- users ]

-- our test database
userDB :: UserDB
userDB = createUserDB
  [ User "john" "shhhh" "john.com"
  , User "foo" "bar" "foobar.net"
  ]
```

Our API will contain a single endpoint, returning the authenticated
user's own website.

``` haskell
-- a 'GET /mysite' endpoint, protected by basic authentication
type API = BasicAuth "People's websites" User :> "mysite" :> Get '[JSON] Website

{- if there were more endpoints to be protected, one could write:
type API = BasicAuth "People's websites" User :>
    ( "foo" :> Get '[JSON] Foo
 :<|> "bar" :> Get '[JSON] Bar
    )
-}

api :: Proxy API
api = Proxy

server :: Server API
server usr = return (site usr)
```

In order to protect our endpoint (`"mysite" :> Get '[JSON] Website`), we simply
drop the `BasicAuth` combinator in front of it. Its first parameter,
`"People's websites"` in our example, is the realm, which is an arbitrary string
identifying the protected resources. The second parameter, `User` in our example,
corresponds to the type we want to use to represent authenticated users. It could
be anything.

When using `BasicAuth` in an API, the server implementation "gets" an argument
of the authenticated user type used with `BasicAuth`, `User` in our case, in the
"corresponding spot". In this example, the server implementation simply returns
the `site` field of the authenticated user. More realistic applications would
have endpoints that take other arguments and where a lot more logic would
be implemented. But in a sense, `BasicAuth` adds an argument just like `Capture`,
`QueryParam`, `ReqBody` and friends. But instead of performing some form of
decoding logic behind the scenes, servant runs some "basic auth check" that the
user provides.

In our case, we need access to our user database, so we simply
take it as an argument. A more serious implementation would probably take
a database connection or even a connection pool.

``` haskell
-- provided we are given a user database, we can supply
-- a function that checks the basic auth credentials
-- against our database.
checkBasicAuth :: UserDB -> BasicAuthCheck User
checkBasicAuth db = BasicAuthCheck $ \basicAuthData ->
  let username = decodeUtf8 (basicAuthUsername basicAuthData)
      password = decodeUtf8 (basicAuthPassword basicAuthData)
  in
  case Map.lookup username db of
    Nothing -> return NoSuchUser
    Just u  -> if pass u == password
               then return (Authorized u)
               else return BadPassword
```

This check simply looks up the user in the "database" and makes sure the
right password was used. For reference, here are the definitions of
`BasicAuthResult` and `BasicAuthCheck`:

```
-- | The result of authentication/authorization
data BasicAuthResult usr
  = Unauthorized
  | BadPassword
  | NoSuchUser
  | Authorized usr
  deriving (Eq, Show, Read, Generic, Typeable, Functor)

-- | Datatype wrapping a function used to check authentication.
newtype BasicAuthCheck usr = BasicAuthCheck
  { unBasicAuthCheck :: BasicAuthData
                     -> IO (BasicAuthResult usr)
  }
  deriving (Generic, Typeable, Functor)
```

This is all great, but how is our `BasicAuth` combinator supposed to know
that it should use our `checkBasicAuth` from above? The answer is that it
simply expects to find a `BasicAuthCheck` value for the right user type in
the `Context` with which we serve the application, where `Context` is just
servant's way to allow users to communicate some configuration of sorts to
combinators. It is nothing more than an heterogeneous list and we can create
a context with our auth check and run our application with it with the following
code:

``` haskell
runApp :: UserDB -> IO ()
runApp db = run 8080 (serveWithContext api ctx server)

  where ctx = checkBasicAuth db :. EmptyContext
```

`ctx` above is just a context with one element, `checkBasicAuth db`,
whose type is `BasicAuthCheck User`. In order to say that we want to serve our
application using the supplied context, we just have to use `serveWithContext`
in place of `serve`.

Finally, let's derive a client to this endpoint as well in order to see our
server in action!

``` haskell
getSite :: BasicAuthData -> ClientM Website
getSite = client api

main :: IO ()
main = do
  mgr <- newManager defaultManagerSettings
  bracket (forkIO $ runApp userDB) killThread $ \_ ->
    runClientM (getSite u) (mkClientEnv mgr (BaseUrl Http "localhost" 8080 ""))
      >>= print

  where u = BasicAuthData "foo" "bar"
```

This program prints `Right "foobar.net"`, as expected. Feel free to change this
code and see what happens when you specify credentials that are not in the
database.

The entire program covered here is available as a literate Haskell file
[here](https://github.com/haskell-servant/servant/tree/master/doc/cookbook/basic-auth),
along with a `cabal` project.
