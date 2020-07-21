# Hoist Server With Context for Custom Monads

In this example we'll combine some of the patterns we've seen in other examples
in order to demonstrate using a custom monad with Servant's `Context` and the function
`hoistServerWithContext`.

`hoistServerWithContext` is a pattern you may encounter if you are trying to use a library such as
[servant-auth-server](https://hackage.haskell.org/package/servant-auth-server) along
with your own custom monad.

In this example, our custom monad will be based on the commonly used `ReaderT env IO a` stack.
We'll create an `AppCtx` to represent our `env` and include some logging utilities as well as
other variables we'd like to have available.

In addition, in order to demonstrate a custom `Context`, we'll also include authentication in
our example. As noted previously (in [jwt-and-basic-auth](../jwt-and-basic-auth/JWTAndBasicAuth.lhs)),
while basic authentication comes with Servant itself,
[servant-auth](https://hackage.haskell.org/package/servant-auth) and
[servant-auth-server](https://hackage.haskell.org/package/servant-auth-server)
packages are needed for JWT-based authentication.

Finally, we're going to use [fast-logger](http://hackage.haskell.org/package/fast-logger)
for our logging example below.

This recipe uses the following ingredients:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Prelude ()
import Prelude.Compat

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.Aeson
import Data.Default
import Data.Proxy
import Data.Text
import Data.Time.Clock ( UTCTime, getCurrentTime )
import GHC.Generics
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.RequestLogger.JSON
import Servant as S
import Servant.Auth as SA
import Servant.Auth.Server as SAS
import System.Log.FastLogger                      ( ToLogStr(..)
                                                  , LoggerSet
                                                  , defaultBufSize
                                                  , newStdoutLoggerSet
                                                  , flushLogStr
                                                  , pushLogStrLn )


port :: Int
port = 3001
```

## Custom Monad

Let's say we'd like to create a custom monad based on `ReaderT env` in order to hold
access to a config object as well as some logging utilities.

With that, we could define an `AppCtx` and `AppM` like this:

```haskell
type AppM = ReaderT AppCtx Handler

data AppCtx = AppCtx {
  _getConfig   :: SiteConfig
  , _getLogger :: LoggerSet
  }

data SiteConfig = SiteConfig {
  environment     :: !Text
  , version       :: !Text
  , adminUsername :: !Text
  , adminPasswd   :: !Text
} deriving (Generic, Show)
```

This `SiteConfig` is a simple example: it refers to our deployment environment as well as an
application version. For instance, we may do something different based on the environment our app is
deployed into. When emitting log messages, we may want to include information about
the deployed version of our application.

In addition, we're going to identify a single admin user in our config and use
that definition to authenticate requests inside our handlers. This is not too
flexible (and probably not too secure...), but it works as a simple example.

## Logging

A common contemporary pattern is to emit log messages as JSON for later ingestion
into a database like Elasticsearch.

To emit JSON log messages, we'll create a `LogMessage` object and make it so we can turn it
into a JSON-encoded `LogStr` (a type from `fast-logger`).

```haskell
data LogMessage = LogMessage {
  message        :: !Text
  , timestamp    :: !UTCTime
  , level        :: !Text
  , lversion     :: !Text
  , lenvironment :: !Text
} deriving (Eq, Show, Generic)

instance FromJSON LogMessage
instance ToJSON LogMessage where
  toEncoding = genericToEncoding defaultOptions

instance ToLogStr LogMessage where
  toLogStr = toLogStr . encode
```

Eventually, when we'd like to emit a log message inside one of our Handlers, it'll look like this:

```haskell
sampleHandler :: AppM LogMessage
sampleHandler = do
    config <- asks _getConfig
    logset <- asks _getLogger

    tstamp <- liftIO getCurrentTime
    let logMsg = LogMessage { message = "let's do some logging!"
                            , timestamp = tstamp
                            , level = "info"
                            , lversion = version config
                            , lenvironment = environment config
                            }
    -- emit log message
    liftIO $ pushLogStrLn logset $ toLogStr logMsg
    -- return handler result (for simplicity, result is also a LogMessage)
    pure logMsg
```

## Authentication

To demonstrate the other part of this recipe, we are going to use a simple
representation of a user, someone who may have access to an admin section of our site:

```haskell
data AdminUser = AdminUser { name :: Text }
   deriving (Eq, Show, Read, Generic)
```

The following instances are needed for JWT:

```haskell
instance ToJSON AdminUser
instance FromJSON AdminUser
instance SAS.ToJWT AdminUser
instance SAS.FromJWT AdminUser
```

## API

Now we can define our API.

We'll have an `admin` endpoint and a `login` endpoint that takes a `LoginForm`:

```haskell
type AdminApi =
  "admin" :> Get '[JSON] LogMessage

type LoginApi =
  "login"
      :> ReqBody '[JSON] LoginForm
      :> Post '[JSON] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] LogMessage)

data LoginForm = LoginForm {
  username :: Text
  , password :: Text
} deriving (Eq, Show, Generic)

instance ToJSON LoginForm
instance FromJSON LoginForm
```

We can combine both APIs into one like so:

```haskell
type AdminAndLogin auths = (SAS.Auth auths AdminUser :> AdminApi) :<|> LoginApi
```

## Server

When we define our server, we'll have to define handlers for the `AdminApi` and the `LoginApi` and
we'll have to supply `JWTSettings` and `CookieSettings` so our `login` handler can authenticate users:

```haskell
adminServer :: SAS.CookieSettings -> SAS.JWTSettings -> ServerT (AdminAndLogin auths) AppM
adminServer cs jwts = adminHandler :<|> loginHandler cs jwts
```

The `admin` route should receive an authenticated `AdminUser` as an argument
or it should return a `401`:

```haskell
adminHandler :: AuthResult AdminUser -> AppM LogMessage
adminHandler (SAS.Authenticated adminUser) = do
    config <- asks _getConfig
    logset <- asks _getLogger

    tstamp <- liftIO getCurrentTime
    let logMsg = LogMessage { message = "Admin User accessing admin: " <> name adminUser
                            , timestamp = tstamp
                            , level = "info"
                            , lversion = version config
                            , lenvironment = environment config
                            }
    -- emit log message
    liftIO $ pushLogStrLn logset $ toLogStr logMsg
    -- return handler result (for simplicity, result is a LogMessage)
    pure logMsg
adminHandler _ = throwError err401
```

By contrast, the `login` handler is waiting for a `POST` with a login form.

If login is successful, it will set session cookies and return a value.

Here we're going to include lots of log messages:

```haskell
loginHandler :: CookieSettings
             -> JWTSettings
             -> LoginForm
             -> AppM (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] LogMessage)
loginHandler cookieSettings jwtSettings form = do
  config     <- asks _getConfig
  logset     <- asks _getLogger

  tstamp <- liftIO getCurrentTime
  let logMsg = LogMessage { message = "AdminUser login attempt failed!"
                          , timestamp = tstamp
                          , level = "info"
                          , lversion = version config
                          , lenvironment = environment config
                          }
  case validateLogin config form of
    Nothing -> do
      liftIO $ pushLogStrLn logset $ toLogStr logMsg
      throwError err401
    Just usr -> do
      mApplyCookies <- liftIO $ SAS.acceptLogin cookieSettings jwtSettings usr
      case mApplyCookies of
        Nothing           -> do
          liftIO $ pushLogStrLn logset $ toLogStr logMsg
          throwError err401
        Just applyCookies -> do
          let successMsg = logMsg{message = "AdminUser successfully authenticated!"}
          liftIO $ pushLogStrLn logset $ toLogStr successMsg
          pure $ applyCookies successMsg
loginHandler _ _ _ = throwError err401

validateLogin :: SiteConfig -> LoginForm -> Maybe AdminUser
validateLogin config (LoginForm uname passwd ) =
  if (uname == adminUsername config) && (passwd == adminPasswd config)
    then Just $ AdminUser uname
    else Nothing
```

## `serveWithContext` and `hoistServerWithContext`

In order to build a working server, we'll need to `hoist` our custom monad
into Servant's Handler monad. We'll also need to pass in the proper context to ensure
authentication will work.

This will require both `serveWithContext` and `hoistServerWithContext`.

Let's define the function which will create our `Application`:

```haskell
adminLoginApi :: Proxy (AdminAndLogin '[JWT])
adminLoginApi = Proxy

mkApp :: Context '[SAS.CookieSettings, SAS.JWTSettings] -> CookieSettings -> JWTSettings -> AppCtx -> Application
mkApp cfg cs jwts ctx =
  serveWithContext adminLoginApi cfg $
    hoistServerWithContext adminLoginApi (Proxy :: Proxy '[SAS.CookieSettings, SAS.JWTSettings])
      (flip runReaderT ctx) (adminServer cs jwts)
```

One footnote: because we'd like our logs to be in JSON form, we'll also create a `Middleware` object
so that `Warp` *also* will emit logs as JSON. This will ensure *all* logs are emitted as JSON:

```haskell
jsonRequestLogger :: IO Middleware
jsonRequestLogger =
  mkRequestLogger $ def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }
```

We now have all the pieces we need to serve our application inside a `main` function:

```haskell
main :: IO ()
main = do
  -- typically, we'd create our config from environment variables
  -- but we're going to just make one here
  let config = SiteConfig "dev" "1.0.0" "admin" "secretPassword"

  warpLogger <- jsonRequestLogger
  appLogger <- newStdoutLoggerSet defaultBufSize

  tstamp <- getCurrentTime
  myKey <- generateKey

  let lgmsg = LogMessage {
    message = "My app starting up!"
    , timestamp = tstamp
    , level = "info"
    , lversion = version config
    , lenvironment = environment config
  }
  pushLogStrLn appLogger (toLogStr lgmsg) >> flushLogStr appLogger

  let ctx = AppCtx config appLogger

      warpSettings = Warp.defaultSettings
      portSettings = Warp.setPort port warpSettings
      settings = Warp.setTimeout 55 portSettings
      jwtCfg = defaultJWTSettings myKey
      cookieCfg = if environment config == "dev"
                  then defaultCookieSettings{cookieIsSecure=SAS.NotSecure}
                  else defaultCookieSettings
      cfg = cookieCfg :. jwtCfg :. EmptyContext

  Warp.runSettings settings $ warpLogger $ mkApp cfg cookieCfg jwtCfg ctx
```


## Usage

Now we can run it and try it out with `curl`. In one terminal, let's run our application
and see what our log output looks like:

```$ ./cookbook-hoist-server-with-context
{"message":"My app starting up!","timestamp":"2018-10-04T00:33:12.482568Z","level":"info","lversion":"1.0.0","lenvironment":"dev"}
```

In another terminal, let's ensure that it fails with `err401` if
we're not authenticated:

```
$ curl -v 'http://localhost:3001/admin'
…
< HTTP/1.1 401 Unauthorized
```

```
$ curl -v -XPOST 'http://localhost:3001/login' \
  -H "Content-Type:application/json" \
  -d '{"username": "bad", "password": "wrong"}'
…
< HTTP/1.1 401 Unauthorized
```

And in the other terminal with our log messages (from our JSON `Middleware`):

```
{"time":"03/Oct/2018:17:35:56 -0700","response":{"status":401,"size":null,"body":""},"request":{"httpVersion":"1.1","path":"/admin","size":0,"body":"","durationMs":0.22,"remoteHost":{"hostAddress":"127.0.0.1","port":51029},"headers":[["Host","localhost:3001"],["User-Agent","curl/7.60.0"],["Accept","*/*"]],"queryString":[],"method":"GET"}}
```

Now let's see that authentication works, and that we get JWTs:

```
$ curl -v -XPOST 'http://localhost:3001/login' \
  -H "Content-Type:application/json" \
  -d '{"username": "admin", "password": "secretPassword"}'
…
< HTTP/1.1 200 OK
...
< Server: Warp/3.2.25
< Content-Type: application/json;charset=utf-8
< Set-Cookie: JWT-Cookie=eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsibmFtZSI6ImFkbWluIn19.SIoRcABKSO4mXnRifzqPWlHJUhVwuy32Qon7s1E_c3vHOsLXdXyX4V4eXOw9tMFoeIqgsXMZucqoFb36vAdKwQ; Path=/; HttpOnly; SameSite=Lax
< Set-Cookie: XSRF-TOKEN=y5PmrYHX3ywFUCwGRQqHh1TDheTLiQpwRQB3FFRd8N4=; Path=/
...
{"message":"AdminUser succesfully authenticated!","timestamp":"2018-10-04T00:37:44.455441Z","level":"info","lversion":"1.0.0","lenvironment":"dev"}
```

And in the other terminal with our log messages (note that logging out passwords is insecure...):

```
{"message":"AdminUser succesfully authenticated!","timestamp":"2018-10-04T00:37:44.455441Z","level":"info","lversion":"1.0.0","lenvironment":"dev"}
{"time":"03/Oct/2018:17:37:44 -0700","response":{"status":200,"size":null,"body":null},"request":{"httpVersion":"1.1","path":"/login","size":51,"body":"{\"username\": \"admin\", \"password\": \"secretPassword\"}","durationMs":0.23,"remoteHost":{"hostAddress":"127.0.0.1","port":51044},"headers":[["Host","localhost:3001"],["User-Agent","curl/7.60.0"],["Accept","*/*"],["Content-Type","application/json"],["Content-Length","51"]],"queryString":[],"method":"POST"}}
```

Finally, let's make sure we can access a protected resource with our tokens:

```
$ export jwt=eyJhbGciOiJIUzUxMiJ9.eyJkYXQiOnsibmFtZSI6ImFkbWluIn19.SIoRcABKSO4mXnRifzqPWlHJUhVwuy32Qon7s1E_c3vHOsLXdXyX4V4eXOw9tMFoeIqgsXMZucqoFb36vAdKwQ
$ curl -v \
  -H "Authorization: Bearer $jwt" \
  'http://localhost:3001/admin'
…
< HTTP/1.1 200 OK
{"message":"Admin User accessing admin: admin","timestamp":"2018-10-04T00:58:07.216605Z","level":"info","lversion":"1.0.0","lenvironment":"dev"}
```

And we should see this message logged-out as well:

```
{"message":"Admin User accessing admin: admin","timestamp":"2018-10-04T00:58:07.216605Z","level":"info","lversion":"1.0.0","lenvironment":"dev"}
```

This program is available as a cabal project
[here](https://github.com/haskell-servant/servant/tree/master/doc/cookbook/hoist-server-with-context).
