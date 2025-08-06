# Overview

This doc will walk through a single-module implementation of a servant API connecting to a MySQL database. It will also include some basic CRUD operations.

Once you can wrap your head around this implementation, understanding more complex features like resource pools would be beneficial next steps.

The only *prerequisite* is that you have a MySQL database open on port 3306 of your machine. Docker is an easy way to manage this.

## Setup

- The mysql database should be up and running on 127.0.0.1:3306

- Our API will be exposed on localhost:8080

## REST actions available

*Get all people*

```
/people    GET
```

*Get person by ID*

```
/people/:id    GET
```

*Insert a new person*

```
/people    POST

{
  "name": "NewName",
  "age": 24
}
```

*Delete a person*

```
/people/:id    DELETE
```

## Other notes

At the time of writing this issue may occur when building your project:

```
setup: Missing dependencies on foreign libraries:
* Missing (or bad) C libraries: ssl, crypto
```

If using stack, this can be fixed by adding the following lines to your `stack.yaml`:

```
extra-include-dirs:
- /usr/local/opt/openssl/include
extra-lib-dirs:
- /usr/local/opt/openssl/lib
```

Or for cabal, running your builds with these configurations passed as options.

## Implementation: Main.hs

Let's jump in:

```haskell
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Lib where

import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Logger         (NoLoggingT (..))
import           Control.Monad.Trans.Reader   (runReaderT)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Data.Aeson as JSON
import           Data.Int                     (Int64 (..))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           Database.Persist
import           Database.Persist.MySQL       (ConnectInfo (..),
                                               SqlBackend (..),
                                               defaultConnectInfo, fromSqlKey, runMigration,
                                               runSqlPool, toSqlKey, withMySQLConn)
import           Database.Persist.Sql         (SqlPersistT, runSqlConn)
import           Database.Persist.TH          (mkMigrate, mkPersist,
                                               persistLowerCase, share,
                                               sqlSettings)
import           Database.Persist.Types       (PersistValue(PersistInt64))
import           Servant                      (Handler, throwError)

import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API
import           System.Environment           (getArgs)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person json
    Id   Int Primary Unique
    name Text
    age  Text
    deriving Eq Show Generic
|]

type Api =
       "person" :> Get '[JSON] [Person]
  :<|> "person" :> Capture "id" Int :> Get '[JSON] Person
  :<|> "person" :> Capture "id" Int :> Delete '[JSON] ()
  :<|> "person" :> ReqBody '[JSON] Person :> Post '[JSON] Person

apiProxy :: Proxy Api
apiProxy = Proxy

app :: Application
app = serve apiProxy server

-- Run a database operation, and lift the result into a Handler.
-- This minimises usage of IO operations in other functions
runDB :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> Handler a
runDB a = liftIO $ runNoLoggingT $ runResourceT $ withMySQLConn connInfo $ runSqlConn a

-- Change these out to suit your local setup
connInfo :: ConnectInfo
connInfo = defaultConnectInfo { connectHost = "127.0.0.1", connectUser = "root", connectPassword = "abcd", connectDatabase = "test-database" }

doMigration :: IO ()
doMigration = runNoLoggingT $ runResourceT $ withMySQLConn connInfo $ runReaderT $ runMigration migrateAll

server :: Server Api
server =
  personGET :<|>
  personGETById :<|>
  personDELETE :<|>
  personPOST
    where
        personGET = selectPersons
        personGETById id = selectPersonById id
        personDELETE id = deletePerson id
        personPOST personJson = createPerson personJson

selectPersons :: Handler [Person]
selectPersons = do
  personList <- runDB $ selectList [] []
  pure $ map (\(Entity _ u) -> u) personList

selectPersonById :: Int -> Handler Person
selectPersonById id = do
  sqlResult <- runDB $ get $ PersonKey id
  case sqlResult of
    Just person -> pure person
    Nothing -> throwError err404 { errBody = JSON.encode "Person with ID not found." }

createPerson :: Person -> Handler Person
createPerson person = do
  attemptCreate <- runDB $ insert person
  case attemptCreate of
    PersonKey k -> pure person
    _           -> throwError err503 { errBody = JSON.encode "Could not create Person." }

deletePerson :: Int -> Handler ()
deletePerson id = do runDB $ delete $ PersonKey id

startApp :: IO ()
startApp = do
    args <- getArgs
    let arg1 = if not (null args) then Just (head args) else Nothing
    case arg1 of
        Just "migrate" -> doMigration
        _              -> run 8080 app
```

## Sample requests

Assuming that you have the db running and have first run `stack exec run migrate`, the following sample requests will test your API:

*Create a person*

```bash
curl -X POST \
  http://localhost:8080/person \
  -H 'Accept: */*' \
  -H 'Accept-Encoding: gzip, deflate' \
  -H 'Cache-Control: no-cache' \
  -H 'Connection: keep-alive' \
  -H 'Content-Length: 62' \
  -H 'Content-Type: application/json' \
  -H 'Host: localhost:8080' \
  -H 'cache-control: no-cache' \
  -d '{
	"name": "Jake",
	"age": "25"
}'
```

*Get all persons*

```bash
curl -X GET \
  http://localhost:8080/person \
  -H 'Accept: */*' \
  -H 'Accept-Encoding: gzip, deflate' \
  -H 'Cache-Control: no-cache' \
  -H 'Connection: keep-alive' \
  -H 'Content-Length: 33' \
  -H 'Content-Type: application/json' \
  -H 'Host: localhost:8080' \
  -H 'cache-control: no-cache'
```

*Get person by ID*

```bash
curl -X GET \
  http://localhost:8080/person/1 \
  -H 'Accept: */*' \
  -H 'Accept-Encoding: gzip, deflate' \
  -H 'Cache-Control: no-cache' \
  -H 'Connection: keep-alive' \
  -H 'Content-Type: application/json' \
  -H 'Host: localhost:8080' \
  -H 'cache-control: no-cache'
```
