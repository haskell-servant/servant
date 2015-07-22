{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.JSSpec where

import Data.Either (isRight)
import Data.Proxy
import Language.ECMAScript3.Parser (parseFromString)
import Test.Hspec

import Servant.API
import Servant.JS
import qualified Servant.JS.Vanilla as JS
import qualified Servant.JS.JQuery as JQ
import qualified Servant.JS.Angular as NG
import Servant.JSSpec.CustomHeaders

type TestAPI = "simple" :> ReqBody '[JSON,FormUrlEncoded] String :> Post '[JSON] Bool
          :<|> "has.extension" :> Get '[FormUrlEncoded,JSON] Bool

type TopLevelRawAPI = "something" :> Get '[JSON] Int
                  :<|> Raw

type HeaderHandlingAPI = "something" :> Header "Foo" String
                                     :> Get '[JSON] Int

type CustomAuthAPI = "something" :> Authorization "Basic" String
                                 :> Get '[JSON] Int

type CustomHeaderAPI = "something" :> MyLovelyHorse String
                                   :> Get '[JSON] Int

type CustomHeaderAPI2 = "something" :> WhatsForDinner String
                                    :> Get '[JSON] Int

headerHandlingProxy :: Proxy HeaderHandlingAPI
headerHandlingProxy = Proxy

customAuthProxy :: Proxy CustomAuthAPI
customAuthProxy = Proxy

customHeaderProxy :: Proxy CustomHeaderAPI
customHeaderProxy = Proxy

customHeaderProxy2 :: Proxy CustomHeaderAPI2
customHeaderProxy2 = Proxy

data TestNames = Vanilla
               | VanillaCustom
               | JQuery
               | JQueryCustom
               | Angular
               | AngularCustom
                 deriving (Show, Eq)

customOptions :: CommonGeneratorOptions
customOptions = defCommonGeneratorOptions {
            successCallback = "okCallback",
            errorCallback = "errorCallback"
        }
                 
spec :: Spec
spec = describe "Servant.JQuery" $ do
    (generateJSSpec Vanilla JS.generateVanillaJS)
    (generateJSSpec VanillaCustom $ JS.generateVanillaJSWith customOptions)
    (generateJSSpec JQuery JQ.generateJQueryJS)
    (generateJSSpec JQueryCustom $ JQ.generateJQueryJSWith customOptions)
    (generateJSSpec Angular $ NG.generateAngularJS NG.defAngularOptions)
    (generateJSSpec AngularCustom $ (NG.generateAngularJSWith NG.defAngularOptions) customOptions)
    
    (angularSpec Angular)
    (angularSpec AngularCustom)

angularSpec :: TestNames -> Spec    
angularSpec test = describe specLabel $ do
    it "should implement a service globally" $ do
        let jsText = genJS $ listFromAPI (Proxy :: Proxy TestAPI)
        output jsText
        jsText `shouldContain` (".service('" ++ testName ++ "'")
        
    it "should depend on $http service globally" $ do
        let jsText = genJS $ listFromAPI (Proxy :: Proxy TestAPI)
        output jsText
        jsText `shouldContain` ("('" ++ testName ++ "', function($http) {")
        
    it "should not depend on $http service in handlers" $ do
        let jsText = genJS $ listFromAPI (Proxy :: Proxy TestAPI)
        output jsText
        jsText `shouldNotContain` "getsomething($http, "
    where
        specLabel = "generateJS(" ++ (show test) ++ ")"
        --output = putStrLn
        output _ = return ()
        testName = "MyService"
        ngOpts = NG.defAngularOptions { NG.serviceName = testName }
        genJS req = NG.wrapInService ngOpts req
    
generateJSSpec :: TestNames -> (AjaxReq -> String) -> Spec
generateJSSpec n gen = describe specLabel $ do
    it "should generate valid javascript" $ do
        let (postSimple :<|> getHasExtension ) = javascript (Proxy :: Proxy TestAPI)
        parseFromString (genJS postSimple) `shouldSatisfy` isRight
        parseFromString (genJS getHasExtension) `shouldSatisfy` isRight
        output $ genJS getHasExtension

    it "should use non-empty function names" $ do
        let (_ :<|> topLevel) = javascript (Proxy :: Proxy TopLevelRawAPI)
        output $ genJS (topLevel "GET")
        parseFromString (genJS $ topLevel "GET") `shouldSatisfy` isRight

    it "should handle simple HTTP headers" $ do
        let jsText = genJS $ javascript headerHandlingProxy
        output jsText
        parseFromString jsText `shouldSatisfy` isRight
        jsText `shouldContain` "headerFoo"
        jsText `shouldContain`  (header n "Foo" $ "headerFoo")

    it "should handle complex HTTP headers" $ do
        let jsText = genJS $ javascript customAuthProxy
        output jsText
        parseFromString jsText `shouldSatisfy` isRight
        jsText `shouldContain` "headerAuthorization"
        jsText `shouldContain`  (header n "Authorization" $ "\"Basic \" + headerAuthorization")

    it "should handle complex, custom HTTP headers" $ do
        let jsText = genJS $ javascript customHeaderProxy
        output jsText
        parseFromString jsText `shouldSatisfy` isRight
        jsText `shouldContain` "headerXMyLovelyHorse"
        jsText `shouldContain`  (header n "X-MyLovelyHorse" $ "\"I am good friends with \" + headerXMyLovelyHorse")

    it "should handle complex, custom HTTP headers (template replacement)" $ do
        let jsText = genJS $ javascript customHeaderProxy2
        output jsText
        parseFromString jsText `shouldSatisfy` isRight
        jsText `shouldContain` "headerXWhatsForDinner"
        jsText `shouldContain`  (header n "X-WhatsForDinner" $ "\"I would like \" + headerXWhatsForDinner + \" with a cherry on top.\"")

    it "can generate the whole javascript code string at once with jsForAPI" $ do
        let jsStr = jsForAPI (Proxy :: Proxy TestAPI) gen
        parseFromString jsStr `shouldSatisfy` isRight
    where
        specLabel = "generateJS(" ++ (show n) ++ ")"
        --output = print
        output _ = return ()
        genJS req = generateJS req gen
        header :: TestNames -> String -> String -> String
        header v headerName headerValue
            | v `elem` [Vanilla, VanillaCustom] = "xhr.setRequestHeader(\"" ++ headerName ++ "\", " ++ headerValue ++ ");\n"
            | otherwise = "headers: { \"" ++ headerName ++ "\": " ++ headerValue ++ " }\n"
        --header _ _ _ = "Not Implemented"
