{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.Utils.StaticFilesSpec where

import           Control.Exception         (bracket)
import           Data.Proxy                (Proxy (Proxy))
import           Network.Wai               (Application)
import           System.Directory          (createDirectory,
                                            getCurrentDirectory,
                                            setCurrentDirectory)
import           System.IO.Temp            (withSystemTempDirectory)
import           Test.Hspec                (Spec, around_, describe, it)
import           Test.Hspec.Wai            (get, shouldRespondWith, with)

import           Servant.API               (JSON)
import           Servant.API.Alternative   ((:<|>) ((:<|>)))
import           Servant.API.Capture       (Capture)
import           Servant.API.Get           (Get)
import           Servant.API.Raw           (Raw)
import           Servant.API.Sub           ((:>))
import           Servant.Server            (Server, serve)
import           Servant.ServerSpec        (Person (Person))
import           Servant.Utils.StaticFiles (serveDirectory)

type Api =
       "dummy_api" :> Capture "person_name" String :> Get '[JSON] Person
  :<|> "static" :> Raw


api :: Proxy Api
api = Proxy

app :: Application
app = serve api server

server :: Server Api
server =
       (\ name_ -> return (Person name_ 42))
  :<|> serveDirectory "static"

withStaticFiles :: IO () -> IO ()
withStaticFiles action = withSystemTempDirectory "servant-test" $ \ tmpDir ->
  bracket (setup tmpDir) teardown (const action)
 where
  setup tmpDir = do
    outer <- getCurrentDirectory
    setCurrentDirectory tmpDir
    createDirectory "static"
    writeFile "static/foo.txt" "bar"
    writeFile "static/index.html" "index"
    return outer

  teardown outer = do
    setCurrentDirectory outer

spec :: Spec
spec = do
  around_ withStaticFiles $ with (return app) $ do
    describe "serveDirectory" $ do
      it "successfully serves files" $ do
        get "/static/foo.txt" `shouldRespondWith` "bar"

      it "serves the contents of index.html when requesting the root of a directory" $ do
        get "/static/" `shouldRespondWith` "index"
