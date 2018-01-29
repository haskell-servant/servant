# Combining JWT-based authentication with basic access authentication

In this example we will make a service with
[basic HTTP authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
for Haskell clients and other programs, as well as
with [JWT](https://en.wikipedia.org/wiki/JSON_Web_Token)-based
authentication for web browsers. Web browsers will still use basic
HTTP authentication to retrieve JWTs though.

**Warning**: this is insecure when done over plain HTTP,
so [TLS](https://en.wikipedia.org/wiki/Transport_Layer_Security)
should be used.
See [warp-tls](https://hackage.haskell.org/package/warp-tls) for that.

While basic authentication comes with Servant itself,
[servant-auth](https://hackage.haskell.org/package/servant-auth) and
[servant-auth-server](https://hackage.haskell.org/package/servant-auth-server)
packages are needed for the JWT-based one.

This recipe uses the following ingredients:

```haskell
{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds,
  DeriveGeneric, TypeOperators #-}
import Data.Aeson
import GHC.Generics
import Data.Proxy
import System.IO
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp
import Servant as S
import Servant.Client
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import Control.Monad.IO.Class (liftIO)
import Data.Map as M
import Data.ByteString (ByteString)

port :: Int
port = 3001
```


## Authentication

Below is how we'll represent a user: usually user identifier is handy
to keep around, along with their role if
[role-based access control](https://en.wikipedia.org/wiki/Role-based_access_control)
is used, and other commonly needed information, such as an
organization identifier:

```haskell
data AuthenticatedUser = AUser { auID :: Int
                               , auOrgID :: Int
                               } deriving (Show, Generic)
```

The following instances are needed for JWT:

```haskell
instance ToJSON AuthenticatedUser
instance FromJSON AuthenticatedUser
instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser
```

We'll have to use a bit of imagination to pretend that the following
`Map` is a database connection pool:

```haskell
type Login      = ByteString
type Password   = ByteString
type DB         = Map (Login, Password) AuthenticatedUser
type Connection = DB
type Pool a     = a

initConnPool :: IO (Pool Connection)
initConnPool = pure $ fromList [ (("user", "pass"), AUser 1 1)
                               , (("user2", "pass2"), AUser 2 1) ]
```

See the "PostgreSQL connection pool" recipe for actual connection
pooling, and we proceed to an authentication function that would use
our improvised DB connection pool and credentials provided by a user:

```haskell
authCheck :: Pool Connection
          -> BasicAuthData
          -> IO (AuthResult AuthenticatedUser)
authCheck connPool (BasicAuthData login password) = pure $
  maybe SAS.Indefinite Authenticated $ M.lookup (login, password) connPool
```

**Warning**: make sure to use a proper password hashing function in
functions like this: see [bcrypt](https://en.wikipedia.org/wiki/Bcrypt),
[scrypt](https://en.wikipedia.org/wiki/Scrypt),
[pgcrypto](https://www.postgresql.org/docs/current/static/pgcrypto.html).

Unlike `Servant.BasicAuth`, `Servant.Auth` uses `FromBasicAuthData`
type class for the authentication process itself. But since our
connection pool will be initialized elsewhere, we'll have to pass it
somehow: it can be done via a context entry and `BasicAuthCfg` type
family. We can actually pass a function at once, to make it a bit more
generic:


```haskell
type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthenticatedUser)

instance FromBasicAuthData AuthenticatedUser where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData
```


## API

Test API with a couple of endpoints:

```haskell
type TestAPI = "foo" :> Capture "i" Int :> Get '[JSON] ()
               :<|> "bar" :> Get '[JSON] ()
```

We'll use this for server-side functions, listing the allowed
authentication methods using the `Auth` combinator:

```haskell
type TestAPIServer =
  Auth '[SA.JWT, SA.BasicAuth] AuthenticatedUser :> TestAPI
```

But `Servant.Auth.Client` only supports JWT-based authentication, so
we'll have to use regular `Servant.BasicAuth` to derive client
functions that use basic access authentication:

```haskell
type TestAPIClient = S.BasicAuth "test" AuthenticatedUser :> TestAPI
```


## Client

Client code in this setting is the same as it would be with just
`Servant.BasicAuth`, using
[servant-client](https://hackage.haskell.org/package/servant-client):

```haskell
testClient :: IO ()
testClient = do
  mgr <- newManager defaultManagerSettings
  let (foo :<|> _) = client (Proxy :: Proxy TestAPIClient)
                     (BasicAuthData "name" "pass")
  res <- runClientM (foo 42)
    (mkClientEnv mgr (BaseUrl Http "localhost" port ""))
  hPutStrLn stderr $ case res of
    Left err -> "Error: " ++ show err
    Right r -> "Success: " ++ show r
```


## Server

Server code is slightly different -- we're getting `AuthResult` here:

```haskell
server :: Server TestAPIServer
server (Authenticated user) = handleFoo :<|> handleBar
  where
    handleFoo :: Int -> Handler ()
    handleFoo n = liftIO $ hPutStrLn stderr $
      concat ["foo: ", show user, " / ", show n]
    handleBar :: Handler ()
    handleBar = liftIO testClient
```

Catch-all for `BadPassword`, `NoSuchUser`, and `Indefinite`:

```haskell
server _ = throwAll err401
```

With `Servant.Auth`, we'll have to put both `CookieSettings` and
`JWTSettings` into context even if we're not using those, and we'll
put a partially applied `authCheck` function there as well, so that
`FromBasicAuthData` will be able to use it, while it will use our
connection pool. Otherwise it is similar to the usual way:

```haskell
mkApp :: Pool Connection -> IO Application
mkApp connPool = do
  myKey <- generateKey
  let jwtCfg = defaultJWTSettings myKey
      authCfg = authCheck connPool
      cfg = jwtCfg :. defaultCookieSettings :. authCfg :. EmptyContext
      api = Proxy :: Proxy TestAPIServer
  pure $ serveWithContext api cfg server
```

Finally, the main function:

```haskell
main :: IO ()
main = do
  connPool <- initConnPool
  let settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr
                           ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp connPool
```


## Usage

Now we can try it out with `curl`. First of all, let's ensure that it
fails with `err401` if we're not authenticated:

```
$ curl -v 'http://localhost:3001/bar'
…
< HTTP/1.1 401 Unauthorized
```

```
$ curl -v 'http://user:wrong_password@localhost:3001/bar'
…
< HTTP/1.1 401 Unauthorized
```

Now let's see that basic HTTP authentication works, and that we get
JWTs:

```
$ curl -v 'http://user:pass@localhost:3001/bar'
…
< HTTP/1.1 200 OK
…
< Set-Cookie: XSRF-TOKEN=lQE/sb1fW4rZ/FYUQZskI6RVRllG0CWZrQ0d3fXU4X0=; Path=/; Secure
< Set-Cookie: JWT-Cookie=eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiYXVPcmdJRCI6MSwiYXVJRCI6MX19.6ZQba-Co5Ul4wpmU34zXlI75wmasxDfaGRmO3BsOx-ONupX93OBfyYBCIJ3tbWMXKBVVqMDt0Pz-5CakyF2wng; Path=/; HttpOnly; Secure
```

And authenticate using JWTs alone, using the token from `JWT-Cookie`:

```
curl -v -H 'Authorization: Bearer eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsiYXVPcmdJRCI6MSwiYXVJRCI6MX19.6ZQba-Co5Ul4wpmU34zXlI75wmasxDfaGRmO3BsOx-ONupX93OBfyYBCIJ3tbWMXKBVVqMDt0Pz-5CakyF2wng' 'http://localhost:3001/bar'
…
< HTTP/1.1 200 OK
```

This program is available as a cabal project
[here](https://github.com/haskell-servant/servant/tree/master/doc/cookbook/jwt-and-basic-auth).
