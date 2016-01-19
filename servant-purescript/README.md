# servant-purescript

[![Build Status](https://travis-ci.org/anchor/servant-purescript.svg?branch=master)](https://travis-ci.org/anchor/servant-purescript)

This library lets you automatically derive Purescript functions (using jQuery's AJAX capabilities) that let you query each endpoint of a [*servant*](http://haskell-servant.github.io) webservice.

© Anchor 2015

## Purescript dependencies

Projects that use *servant-purescript* will depend on the following Purescript libraries:

* [purescript-affjax](https://github.com/slamdata/purescript-affjax)
* [purescript-arrays](https://github.com/purescript/purescript-arrays)
* [purescript-control](https://github.com/purescript/purescript-control)
* [purescript-either](https://github.com/purescript/purescript-either)
* [purescript-foldable-traversable](https://github.com/purescript/purescript-foldable-traversable)
* [purescript-foreign](https://github.com/purescript/purescript-foreign)
* [purescript-maybe](https://github.com/purescript/purescript-maybe)
* [purescript-monoid](https://github.com/purescript/purescript-monoid)
* [purescript-tuples](https://github.com/purescript/purescript-tuples)

You should be able to get all these libraries if you require `purescript-foreign` on its own.

It's recommended that you use [bower](http://bower.io) to manage dependencies for your project. Use the bower.json file in examples as a reference.

## Usage example

The following example is ported from the example provided with [*servant-jquery*](https://github.com/haskell-servant/servant-jquery).

To build it (and the tests), run the following:

`cabal configure --enable-tests --flags="example"`

And to run:

`cabal run counter`

Here's [more information about the original example](https://github.com/haskell-servant/servant-jquery/tree/master/examples#examples) that will explain how it works and how it should run.

The PureScript version is a direct port of this. The *servant-purescript* bindings are invoked within Javascript.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.List
import Data.Monoid
import Data.Proxy
import GHC.Generics
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.JQuery
import Servant.PureScript
import System.FilePath
import System.FilePath.Glob
import System.Process

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
          :<|> Raw                       -- used for serving static files

testApi :: Proxy TestApi
testApi = Proxy

-- * Server-side handler

-- where our static files reside
www :: FilePath
www = "examples/www"

-- where temporary files reside
tmp :: FilePath
tmp = "examples/temp"

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

writePS :: FilePath -> [AjaxReq] -> IO ()
writePS fp functions = writeFile fp $
    generatePSModule defaultSettings "App" functions

main :: IO ()
main = do
    -- Write the PureScript module
    writePS (tmp </> "api.purs") [ incCounterJS
                                 , currentValueJS
                                 ]

    -- Run bower to import dependencies
    _ <- system "cd examples && bower install"

    (matches, _) <- globDir [compile "examples/bower_components/**/*.purs"] "."

    -- Compile PureScript to JS
    let cmd = "psc "
            <> (intercalate " " $ head matches)
            <> " "
            <> (tmp </> "api.purs")
            <> " > "
            <> (www </> "api.js")

    putStrLn cmd

    _ <- system cmd

    -- setup a shared counter
    cnt <- newCounter

    -- listen to requests on port 8080
    runServer cnt 8080
```

## Another example

There is a simple todo list example that can be built using this command:

`cabal configure --enable-tests --flags="example"`

The example can then be run using this command:

`cabal run todolist`
