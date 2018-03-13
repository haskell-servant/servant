# PostgreSQL connection pool

Let's see how we can write a simple web application that uses a
[PostgreSQL](https://www.postgresql.org/) database to store simple textual
messages, just like in the SQLite cookbook recipe. The main difference,
besides the database technology, is that in this example we will be using
a pool of connections to talk to the database server. The pool abstraction
will be provided by the
[resource-pool](https://hackage.haskell.org/package/resource-pool) library.

As usual, we start with a little bit of throat clearing.

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
import Data.ByteString (ByteString)
import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.Pool
import Database.PostgreSQL.Simple
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp
import Servant
import Servant.Client

type DBConnectionString = ByteString
```

We will only care about a single type here, the messages. We want to
be able to add a new one and retrieve them all, using two different
endpoints.

``` haskell
type Message = String

type API = ReqBody '[PlainText] Message :> Post '[JSON] NoContent
      :<|> Get '[JSON] [Message]

api :: Proxy API
api = Proxy
```

We proceed with a simple function for creating a table
for holding our messages if it doesn't already exist, given
a PostgreSQL connection string.

``` haskell
initDB :: DBConnectionString -> IO ()
initDB connstr = bracket (connectPostgreSQL connstr) close $ \conn -> do
  execute_ conn
    "CREATE TABLE IF NOT EXISTS messages (msg text not null)"
  return ()
```

Next, our server implementation. It will be parametrised (take as
argument) by the pool of database connections that handlers can use to
talk to the PostgreSQL database. The resource pool abstraction allows us
to flexibly set up a whole bunch of PostgreSQL connections tailored to our
needs and then to forget about it all by simply asking for a connection
using `withResource`.

The handlers are straightforward. One takes care of inserting a new
value in the database while the other fetches all messages and returns
them. We also provide a function for serving our web app given a PostgreSQL
connection pool, which simply calls servant-server's `serve` function.

``` haskell
server :: Pool Connection -> Server API
server conns = postMessage :<|> getMessages

  where postMessage :: Message -> Handler NoContent
        postMessage msg = do
          liftIO . withResource conns $ \conn ->
            execute conn
                    "INSERT INTO messages VALUES (?)"
                    (Only msg)
          return NoContent

        getMessages :: Handler [Message]
        getMessages = fmap (map fromOnly) . liftIO $
         withResource conns $ \conn ->
            query_ conn "SELECT msg FROM messages"

runApp :: Pool Connection -> IO ()
runApp conns = run 8080 (serve api $ server conns)
```

We will also need a function for initialising our connection pool.
`resource-pool` is quite configurable, feel free to wander in
[its documentation](https://hackage.haskell.org/package/resource-pool)
to gain a better understanding of how it works and what the configuration
knobs are. I will be using some dummy values in this example.

``` haskell
initConnectionPool :: DBConnectionString -> IO (Pool Connection)
initConnectionPool connStr =
  createPool (connectPostgreSQL connStr)
             close
             2 -- stripes
             60 -- unused connections are kept open for a minute
             10 -- max. 10 connections open per stripe
```

Let's finally derive some clients for our API and use them to
insert two messages and retrieve them in `main`, after setting up
our pool of database connections.

``` haskell
postMsg :: Message -> ClientM NoContent
getMsgs :: ClientM [Message]
postMsg :<|> getMsgs = client api

main :: IO ()
main = do
  -- you could read this from some configuration file,
  -- environment variable or somewhere else instead.
  -- you will need to either change this connection string OR
  -- set some environment variables (see
  -- https://www.postgresql.org/docs/9.5/static/libpq-envars.html)
  -- to point to a running PostgreSQL server for this example to work.
  let connStr = ""
  pool <- initConnectionPool connStr
  initDB connStr
  mgr <- newManager defaultManagerSettings
  bracket (forkIO $ runApp pool) killThread $ \_ -> do
    ms <- flip runClientM (mkClientEnv mgr (BaseUrl Http "localhost" 8080 "")) $ do
      postMsg "hello"
      postMsg "world"
      getMsgs
    print ms
```

This program prints `Right ["hello","world"]` the first time it is executed,
`Right ["hello","world","hello","world"]` the second time and so on.

You could alternatively have the handlers live in `ReaderT (Pool Connection)`
and access the pool using e.g `ask`, but this would be more complicated
than simply taking the pool as argument.

The entire source for this example is available as a cabal project
[here](https://github.com/haskell-servant/servant/tree/master/doc/cookbook/db-postgres-pool).
