# servant-js

![servant](https://raw.githubusercontent.com/haskell-servant/servant/master/servant.png)

This library lets you derive automatically (JQuery based) Javascript functions that let you query each endpoint of a *servant* webservice.

## Example

Read more about the following example [here](https://github.com/haskell-servant/servant/tree/master/servant-jquery/tree/master/examples#examples).

``` haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.JQuery
import System.FilePath

-- * A simple Counter data type
newtype Counter = Counter { value :: Int }
  deriving (Generic, Show, Num)

instance ToJSON Counter

-- * Shared counter operations

-- Creating a counter that starts from 0
newCounter :: IO (TVar Counter)
newCounter = newTVarIO 0

-- Increasing the counter by 1
counterPlusOne :: MonadIO m => TVar Counter -> m Counter
counterPlusOne counter = liftIO . atomically $ do
  oldValue <- readTVar counter
  let newValue = oldValue + 1
  writeTVar counter newValue
  return newValue

currentValue :: MonadIO m => TVar Counter -> m Counter
currentValue counter = liftIO $ readTVarIO counter

-- * Our API type
type TestApi = "counter" :> Post Counter -- endpoint for increasing the counter
          :<|> "counter" :> Get  Counter -- endpoint to get the current value
          :<|> Raw IO Application        -- used for serving static files

testApi :: Proxy TestApi
testApi = Proxy

-- * Server-side handler

-- where our static files reside
www :: FilePath
www = "examples/www"

-- defining handlers
server :: TVar Counter -> Server TestApi
server counter = counterPlusOne counter     -- (+1) on the TVar
            :<|> currentValue counter       -- read the TVar
            :<|> serveDirectory www         -- serve static files

runServer :: TVar Counter -- ^ shared variable for the counter
          -> Int          -- ^ port the server should listen on
          -> IO ()
runServer var port = run port (serve testApi $ server var)

-- * Generating the JQuery code

incCounterJS :<|> currentValueJS :<|> _ = jquery testApi

writeJS :: FilePath -> [AjaxReq] -> IO ()
writeJS fp functions = writeFile fp $
  concatMap generateJS functions

main :: IO ()
main = do
  -- write the JS code to www/api.js at startup
  writeJS (www </> "api.js")
          [ incCounterJS, currentValueJS ]

  -- setup a shared counter
  cnt <- newCounter

  -- listen to requests on port 8080
  runServer cnt 8080
```
