{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.Utils.StaticFilesSpec where

import Control.Exception
import Data.Foldable
import Data.List
import Data.Proxy
import Data.String.Conversions
import Network.Wai
import Network.Wai.Test
import System.Directory
import System.IO.Temp
import Test.Hspec hiding (pending)
import Test.Hspec.Wai

import Servant.API.Alternative
import Servant.API.Capture
import Servant.API.Get
import Servant.API.Raw
import Servant.API.Sub
import Servant.Docs
import Servant.Server
import Servant.ServerSpec
import Servant.Utils.StaticFiles

type Api =
       "dummy_api" :> Capture "person_name" String :> Get Person
  :<|> "static" :> Raw
  :<|> "documentation" :> Raw

instance ToCapture (Capture "person_name" String) where
  toCapture _proxy = DocCapture "person_name" "person_name_doc"

api :: Proxy Api
api = Proxy

app :: Application
app = serve api server

server :: Server Api
server =
       (\ name -> return (Person name 42))
  :<|> serveDirectory "static"
  :<|> serveDocumentation api

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

    describe "serveDocumentation" $ do
      it "serves documentation about the expose API" $ do
        response <- get "/documentation"
        liftIO $ forM_ ["person_name" :: String, "static", "dummy_api", "person_name_doc"] $
          \ snippet ->
            (snippet, cs (simpleBody response)) `shouldSatisfy` uncurry isInfixOf
