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
import Servant.JS
import qualified Servant.JS as SJS
import qualified Servant.JS.Vanilla as JS
import qualified Servant.JS.JQuery as JQ
import qualified Servant.JS.Angular as NG
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
type TestApi = "counter" :> Post '[JSON] Counter -- endpoint for increasing the counter
          :<|> "counter" :> Get '[JSON] Counter -- endpoint to get the current value
          :<|> Raw                       -- used for serving static files 

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
incCounterJS :: AjaxReq
currentValueJS :: AjaxReq
incCounterJS :<|> currentValueJS :<|> _ = javascript testApi

writeJS :: JavaScriptGenerator -> FilePath -> [AjaxReq] -> IO ()
writeJS gen fp functions = writeFile fp $
  concatMap (\req -> generateJS req gen) functions
  
writeServiceJS :: FilePath -> [AjaxReq] -> IO ()
writeServiceJS fp functions = writeFile fp $
  NG.wrapInServiceWith (NG.defAngularOptions { NG.serviceName = "counterSvc" })
    (defCommonGeneratorOptions { SJS.moduleName = "counterApp" }) functions
  
main :: IO ()
main = do
  -- write the JS code to www/api.js at startup
  writeJS JQ.generateJQueryJS (www </> "jquery" </> "api.js")
          [ incCounterJS, currentValueJS ]
  writeJS JS.generateVanillaJS (www </> "vanilla" </> "api.js")
          [ incCounterJS, currentValueJS ]
  writeJS (NG.generateAngularJS 
            NG.defAngularOptions) (www </> "angular" </> "api.js")
          [ incCounterJS, currentValueJS ]
  writeServiceJS 
          (www </> "angular" </> "api.service.js")
          [ incCounterJS, currentValueJS ]

  -- setup a shared counter
  cnt <- newCounter

  -- listen to requests on port 8080
  runServer cnt 8080
