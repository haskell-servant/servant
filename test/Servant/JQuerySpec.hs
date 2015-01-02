{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
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

type TopLevelRawAPI = "something" :> Get Int
                  :<|> Raw

spec :: Spec
spec = describe "Servant.JQuery"
    generateJSSpec

generateJSSpec :: Spec
generateJSSpec = describe "generateJS" $ do
    it "should generate valid javascript" $ do
        let (postSimple :<|> getHasExtension ) = jquery (Proxy :: Proxy TestAPI)
        parseFromString (generateJS postSimple) `shouldSatisfy` isRight
        parseFromString (generateJS getHasExtension) `shouldSatisfy` isRight
        print $ generateJS getHasExtension

    it "should use non-empty function names" $ do
        let (_ :<|> topLevel) = jquery (Proxy :: Proxy TopLevelRawAPI)
        print $ generateJS $ topLevel "GET"
        parseFromString (generateJS $ topLevel "GET") `shouldSatisfy` isRight

