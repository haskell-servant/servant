{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.Utils.StaticFilesSpec where

import Control.Exception
import Data.Proxy
import Network.Wai
import System.Directory
import System.IO.Temp
import Test.Hspec hiding (pending)
import Test.Hspec.Wai

import Servant.API.Alternative
import Servant.API.Capture
import Servant.API.Get
import Servant.API.Raw
import Servant.API.Sub
import Servant.Server
import Servant.ServerSpec
import Servant.Utils.StaticFiles

type Api =
       "dummy_api" :> Capture "person_name" String :> Get Person
  :<|> "static" :> Raw


api :: Proxy Api
api = Proxy

app :: Application
app = serve api server

server :: Server Api
server =
       (\ name -> return (Person name 42))
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
        get "/static" `shouldRespondWith` "index"
