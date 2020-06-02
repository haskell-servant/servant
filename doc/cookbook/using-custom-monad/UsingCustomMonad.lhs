# Using a custom monad

In this section we will create an API for a book shelf without any backing DB storage.
We will keep state in memory and share it between requests using `Reader` monad and `STM`.

We start with a pretty standard set of imports and definition of the model:

``` haskell
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

import           Control.Concurrent          (forkIO, killThread)
import           Control.Concurrent.STM.TVar (TVar, newTVar, readTVar,
                                              writeTVar)
import           Control.Exception           (bracket)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.STM           (atomically)
import           Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import           Data.Aeson                  (FromJSON, ToJSON)
import           GHC.Generics                (Generic)
import           Network.HTTP.Client         (defaultManagerSettings,
                                              newManager)
import           Network.Wai.Handler.Warp    (run)

import           Servant
import           Servant.Client

newtype Book = Book String deriving (Show, Generic)
instance ToJSON Book
instance FromJSON Book
```

Now, let's define the API for book storage.
For the sake of simplicity we'll only have methods for getting all books and adding a new one.

``` haskell
type GetBooks = Get '[JSON] [Book]
type AddBook = ReqBody '[JSON] Book :> PostCreated '[JSON] Book
type BooksAPI = "books" :> (GetBooks :<|> AddBook)

api :: Proxy BooksAPI
api = Proxy
```

Next, we define the state and the monad to run our handlers

``` haskell
data State = State
  { books :: TVar [Book]
  }

type AppM = ReaderT State Handler
```

Note that we can't use `State` monad here, because state will not be shared between requests.

We can now define handlers in terms of `AppM`...

```haskell
server :: ServerT BooksAPI AppM
server = getBooks :<|> addBook
  where getBooks :: AppM [Book]
        getBooks = do
          State{books = p} <- ask
          liftIO $ atomically $ readTVar p

        addBook :: Book -> AppM Book
        addBook book = do
          State{books = p} <- ask
          liftIO $ atomically $ readTVar p >>= writeTVar p . (book :)
          return book

```

...and transform `AppM` to `Handler` by simply using `runReaderT`

``` haskell
nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s

app :: State -> Application
app s = serve api $ hoistServer api (nt s) server
```

Finally, we end up with the following program

``` haskell
main :: IO ()
main = do
  let port = 8080
  mgr <- newManager defaultManagerSettings
  initialBooks <- atomically $ newTVar []
  let runApp = run port $ app $ State initialBooks
  bracket (forkIO runApp) killThread $ \_ -> do
    let getBooksClient :<|> addBookClient = client api
    let printBooks = getBooksClient >>= liftIO . print
    _ <- flip runClientM (mkClientEnv mgr (BaseUrl Http "localhost" port "")) $ do
      _ <- printBooks
      _ <- addBookClient $ Book "Harry Potter and the Order of the Phoenix"
      _ <- printBooks
      _ <- addBookClient $ Book "To Kill a Mockingbird"
      _ <- printBooks
      _ <- addBookClient $ Book "The Picture of Dorian Gray"
      printBooks
    return ()
```

When run, it outputs the following:

```
Running cookbook-using-custom-monad...
[]
[Book "Harry Potter and the Order of the Phoenix"]
[Book "To Kill a Mockingbird",Book "Harry Potter and the Order of the Phoenix"]
[Book "The Picture of Dorian Gray",Book "To Kill a Mockingbird",Book "Harry Potter and the Order of the Phoenix"]
```

To use `Raw` endpoints, look at the
[servant-rawm](http://hackage.haskell.org/package/servant-rawm) package.
