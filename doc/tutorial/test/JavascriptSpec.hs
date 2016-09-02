{-# LANGUAGE OverloadedStrings #-}

module JavascriptSpec where

import           Data.List
import           Data.String
import           Data.String.Conversions
import           Test.Hspec
import           Test.Hspec.Wai

import           Javascript

spec :: Spec
spec = do
  describe "apiJS" $ do
    it "is contained verbatim in Javascript.lhs" $ do
      code <- readFile "Javascript.lhs"
      cs apiJS1 `shouldSatisfy` (`isInfixOf` code)
      cs apiJS3 `shouldSatisfy` (`isInfixOf` code)
      cs apiJS4 `shouldSatisfy` (`isInfixOf` code)
      cs apiJS6 `shouldSatisfy` (`isInfixOf` code)

  describe "writeJSFiles" $ do
    it "[not a test] write apiJS to static/api.js" $ do
      writeJSFiles

  describe "app" $ with (return app) $ do
    context "/api.js" $ do
      it "delivers apiJS" $ do
        get "/api.js" `shouldRespondWith` (fromString (cs apiJS1))

    context "/" $ do
      it "delivers something" $ do
        get "" `shouldRespondWith` 200
        get "/" `shouldRespondWith` 200
