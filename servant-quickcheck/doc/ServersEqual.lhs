# Testing that servers behave identically

## Rewriting an application

If you are rewriting, or significantly refactoring, an application, you often
want to ensure that the behaviour of the rewritten application is the same as
that of the old one. Sometimes what the behaviour of the old application is is
not always clear, making the process a difficult and error-prone one.

**servant-quickcheck** can help. It provides a `serversEqual` function that,
given a **servant** API type and two URLs, generates arbitrary requests of the
right type and checks that, for the same request *history*, the two servers
respond identically.

To see how this works, let's re-implement the [Django
Todo-Backend](https://github.com/mihirk/todo-backend-django) application
in **servant**. (`serversEqual` works for non-**servant** applications, though
it's somewhat nicer to use when one of them is written with **servant**.) You
don't need to know anything about Django or Python to follow along; indeed,
part of the fun of it is using `serversEqual` to guide you through
re-implementing code you may not entirely understand.

Looking around the codebase (`urls.py`, `models.py` and `views.py` in
particular) may lead us to a first attempt at a rewrite:

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module ServersEqual where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM as STM
import Control.Monad.IO.Class
import Data.Aeson (ToJSON, FromJSON)
import Data.IntMap as M
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.QuickCheck (serversEqual, BaseUrl(..), Scheme(..))
import Test.QuickCheck (Arbitrary(..))

data Todo = Todo
    { title     :: String
    , completed :: Bool
    , url       :: String
    , order     :: Int
    } deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

type API = TodosAPI :<|> TodoAPI

type TodosAPI
  =   "todos" :>
           ( Get '[JSON] [Todo]
       :<|>  ReqBody '[JSON] Todo :> Post '[JSON] ()
       :<|>  Delete '[JSON] ())

type TodoAPI
  =    "todo" :> Capture "id " Int :>
           ( Get '[JSON] Todo
       :<|>  ReqBody '[JSON] Todo :> Patch '[JSON] ()
       :<|> Delete '[JSON] ())

serverTodos :: DB -> Server TodosAPI
serverTodos tvar = getTodos tvar
              :<|> postTodos tvar
              :<|> deleteAllTodos tvar

{-
serverTodo :: Server TodoAPI
serverTodo id' = getTodo tvar id'
            :<|> patchTodo tvar id'
            :<|> deleteTodo tvar id'
-}

type DB = TVar (M.IntMap Todo)

getTodos :: DB -> Handler [Todo]
getTodos m = liftIO $ M.elems <$> STM.readTVarIO m

postTodos :: DB -> Todo -> Handler ()
postTodos m t = liftIO . STM.atomically $ STM.modifyTVar' m (M.insert (order t) t)

deleteAllTodos :: DB -> Handler ()
deleteAllTodos m = liftIO . STM.atomically $ STM.writeTVar m M.empty

main1 :: IO ()
main1 = do
    db <- STM.newTVarIO M.empty
    tId <- forkIO $ run 8000 $ serve (Proxy :: Proxy TodosAPI) $ serverTodos db
    _ <- serversEqual (Proxy :: Proxy TodosAPI)
                      (BaseUrl Http "localhost" 8000 "")
                      (BaseUrl Http "localhost" 8001 "")
                      1000
    killThread tId
```

(We're keeping the `Todo`s in an `MVar` for simplicity. If this were a
 production application, we'd likely want to use a database.)

Notice that we split up the API into two sub-APIs. Partly this makes things
cleaner and more readable, but there's also a more concrete benefit: we can
start testing that **parts** of the API have been correctly rewritten without
implementing the entire server.

In order to check how we're doing, we need to add an `Arbitrary` instance for
`Todo`:

``` haskell
instance Arbitrary Todo where
  arbitrary = Todo <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
```

Let's try it out. First we need to get the old application running:

``` bash
git clone https://github.com/mihirk/todo-backend-django.git
cd todo-backend-django
virtualenv venv
. ./venv/bin/activate
python manage.py runserver localhost:8001
```

