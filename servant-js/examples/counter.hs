{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Proxy
import           GHC.Generics
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Servant.JS
import qualified Servant.JS               as SJS
import qualified Servant.JS.Angular       as NG
import           System.FilePath

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

type TestApi' = TestApi
           :<|> Raw -- used for serving static files

-- this proxy only targets the proper endpoints of our API,
-- not the static file serving bit
testApi :: Proxy TestApi
testApi = Proxy

-- this proxy targets everything
testApi' :: Proxy TestApi'
testApi' = Proxy

-- * Server-side handler

-- where our static files reside
www :: FilePath
www = "examples/www"

-- defining handlers of our endpoints
server :: TVar Counter -> Server TestApi
server counter = counterPlusOne counter     -- (+1) on the TVar
            :<|> currentValue counter       -- read the TVar

-- the whole server, including static file serving
server' :: TVar Counter -> Server TestApi'
server' counter = server counter
             :<|> serveDirectory www -- serve static files

runServer :: TVar Counter -- ^ shared variable for the counter
          -> Int          -- ^ port the server should listen on
          -> IO ()
runServer var port = run port (serve testApi' $ server' var)

writeServiceJS :: FilePath -> IO ()
writeServiceJS fp =
  writeJSForAPI testApi
                (angularServiceWith (NG.defAngularOptions { NG.serviceName = "counterSvc" })
                                    (defCommonGeneratorOptions { SJS.moduleName = "counterApp" })
                )
                fp

main :: IO ()
main = do
  -- write the JS code to www/api.js at startup
  writeJSForAPI testApi jquery (www </> "jquery" </> "api.js")

  writeJSForAPI testApi vanillaJS (www </> "vanilla" </> "api.js")

  writeJSForAPI testApi (angular defAngularOptions) (www </> "angular" </> "api.js")

  writeJSForAPI testApi axios (www </> "axios" </> "api.js")

  writeServiceJS (www </> "angular" </> "api.service.js")

  -- setup a shared counter
  cnt <- newCounter

  -- listen to requests on port 8080
  runServer cnt 8080
