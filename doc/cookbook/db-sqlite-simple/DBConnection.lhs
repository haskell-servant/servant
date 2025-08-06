# SQLite database

Let's see how we can write a simple web application that uses an
[SQLite](https://www.sqlite.org/) database to store simple textual
messages. As usual, we start with a little bit of throat clearing.

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Database.SQLite.Simple
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
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
for holding our messages if it doesn't already exist.

``` haskell
initDB :: FilePath -> IO ()
initDB dbfile = withConnection dbfile $ \conn ->
  execute_ conn
    "CREATE TABLE IF NOT EXISTS messages (msg text not null)"
```

Next, our server implementation. It will be parametrised (take as an
argument) by the name of the file that contains our SQLite database.
The handlers are straightforward. One takes care of inserting a new
value in the database while the other fetches all messages and returns
them. We also provide a function for serving our web app given an
SQLite database file, which simply calls servant-server's `serve` function.

``` haskell
server :: FilePath -> Server API
server dbfile = postMessage :<|> getMessages

  where postMessage :: Message -> Handler NoContent
        postMessage msg = do
          liftIO . withConnection dbfile $ \conn ->
            execute conn
                    "INSERT INTO messages VALUES (?)"
                    (Only msg)
          pure NoContent

        getMessages :: Handler [Message]
        getMessages = fmap (map fromOnly) . liftIO $
         withConnection dbfile $ \conn ->
            query_ conn "SELECT msg FROM messages"

runApp :: FilePath -> IO ()
runApp dbfile = run 8080 (serve api $ server dbfile)
```

Let's also derive some clients for our API and use them to
insert two messages and retrieve them in `main`.

``` haskell
postMsg :: Message -> ClientM NoContent
getMsgs :: ClientM [Message]
postMsg :<|> getMsgs = client api

main :: IO ()
main = do
  -- you could read this from some configuration file,
  -- environment variable or somewhere else instead.
  let dbfile = "test.db"
  initDB dbfile
  mgr <- newManager defaultManagerSettings
  bracket (forkIO $ runApp dbfile) killThread $ \_ -> do
    ms <- flip runClientM (mkClientEnv mgr (BaseUrl Http "localhost" 8080 "")) $ do
      postMsg "hello"
      postMsg "world"
      getMsgs
    print ms
```

This program prints `Right ["hello","world"]` the first time it is executed,
`Right ["hello","world","hello","world"]` the second time and so on.

The entire source for this example is available as a cabal project
[here](https://github.com/haskell-servant/servant/tree/master/doc/cookbook/db-sqlite-simple).
