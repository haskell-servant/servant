{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.JQuerySpec where

import Data.Either (isRight)
import Data.Proxy
import Language.ECMAScript3.Parser (parseFromString)
import Test.Hspec

import Servant.API
import Servant.JQuery
import Servant.JQuerySpec.CustomHeaders

type TestAPI = [sitemap|
POST    /simple                  String -> Bool
GET     /has.extension           Bool
|]

type TopLevelRawAPI = "something" :> Get Int
                  :<|> Raw

type HeaderHandlingAPI = "something" :> Header "Foo" String
                                     :> Get Int

type CustomAuthAPI = "something" :> Authorization "Basic" String
                                 :> Get Int

type CustomHeaderAPI = "something" :> MyLovelyHorse String
                                   :> Get Int

type CustomHeaderAPI2 = "something" :> WhatsForDinner String
                                    :> Get Int

headerHandlingProxy :: Proxy HeaderHandlingAPI
headerHandlingProxy = Proxy

customAuthProxy :: Proxy CustomAuthAPI
customAuthProxy = Proxy

customHeaderProxy :: Proxy CustomHeaderAPI
customHeaderProxy = Proxy

customHeaderProxy2 :: Proxy CustomHeaderAPI2
customHeaderProxy2 = Proxy

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

    it "should handle simple HTTP headers" $ do
        let jsText = generateJS $ jquery headerHandlingProxy
        print jsText
        parseFromString jsText `shouldSatisfy` isRight
        jsText `shouldContain` "headerFoo"
        jsText `shouldContain` "headers: { \"Foo\": headerFoo }\n"

    it "should handle complex HTTP headers" $ do
        let jsText = generateJS $ jquery customAuthProxy
        print jsText
        parseFromString jsText `shouldSatisfy` isRight
        jsText `shouldContain` "headerAuthorization"
        jsText `shouldContain` "headers: { \"Authorization\": \"Basic \" + headerAuthorization }\n"

    it "should handle complex, custom HTTP headers" $ do
        let jsText = generateJS $ jquery customHeaderProxy
        print jsText
        parseFromString jsText `shouldSatisfy` isRight
        jsText `shouldContain` "headerXMyLovelyHorse"
        jsText `shouldContain` "headers: { \"X-MyLovelyHorse\": \"I am good friends with \" + headerXMyLovelyHorse }\n"

    it "should handle complex, custom HTTP headers (template replacement)" $ do
        let jsText = generateJS $ jquery customHeaderProxy2
        print jsText
        parseFromString jsText `shouldSatisfy` isRight
        jsText `shouldContain` "headerXWhatsForDinner"
        jsText `shouldContain` "headers: { \"X-WhatsForDinner\": \"I would like \" + headerXWhatsForDinner + \" with a cherry on top.\" }\n"
