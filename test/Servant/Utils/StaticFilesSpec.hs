{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Servant.Utils.StaticFilesSpec where

import Test.Hspec hiding (pending)
import System.IO.Temp
import Test.Hspec.Wai
import Network.Wai
import Data.Proxy
import System.Directory
import Control.Exception

import Servant.Utils.StaticFiles
import Servant.API.Sub
import Servant.API.Raw
import Servant.Server

type Api =
       "static" :> Raw

app :: Application
app = serve (Proxy :: Proxy Api) server

server :: Server Api
server = serveDirectory "static"

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
  around_ withStaticFiles $ with (return app) $
    describe "serveDirectory" $ do
      it "successfully serves files" $ do
        get "/static/foo.txt" `shouldRespondWith` "bar"

      it "serves the contents of index.html when requesting the root of a directory" $ do
        get "/static" `shouldRespondWith` "index"
