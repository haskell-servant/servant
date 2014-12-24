{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.JQuerySpec where

import Data.Either (isRight)
import Data.Proxy
import Language.ECMAScript3.Parser (parseFromString)
import Test.Hspec

import Servant.API
import Servant.JQuery

type TestAPI = [sitemap|
POST    /simple                  String -> Bool
GET     /has.extension           Bool
|]

spec :: Spec
spec = describe "Servant.JQuery"
    generateJSSpec

generateJSSpec :: Spec
generateJSSpec = describe "generateJS" $
    it "should always generate valid javascript" $ do
        let (postSimple :<|> getHasExtension) = jquery (Proxy :: Proxy TestAPI)
        parseFromString (generateJS postSimple) `shouldSatisfy` isRight
        parseFromString (generateJS getHasExtension) `shouldSatisfy` isRight

