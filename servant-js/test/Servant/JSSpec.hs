{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.JSSpec where

import           Data.Either                  (isRight)
#if !MIN_VERSION_base(4,8,0)
import           Data.Monoid                  ((<>),mconcat)
#else
import           Data.Monoid                  ((<>))
#endif
import           Data.Proxy
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Language.ECMAScript3.Parser  (program, parse)
import           Test.Hspec  hiding (shouldContain, shouldNotContain)

import           Servant.API.Internal.Test.ComprehensiveAPI
import           Servant.JS
import           Servant.JS.Internal
import qualified Servant.JS.Angular           as NG
import qualified Servant.JS.Axios             as AX
import qualified Servant.JS.JQuery            as JQ
import qualified Servant.JS.Vanilla           as JS
import           Servant.JSSpec.CustomHeaders

-- * comprehensive api

-- This declaration simply checks that all instances are in place.
_ = jsForAPI comprehensiveAPI vanillaJS :: Text

-- * specs

type TestAPI = "simple" :> ReqBody '[JSON,FormUrlEncoded] Text :> Post '[JSON] Bool
          :<|> "has.extension" :> Get '[FormUrlEncoded,JSON] Bool

type TopLevelRawAPI = "something" :> Get '[JSON] Int
                  :<|> Raw

type HeaderHandlingAPI = "something" :> Header "Foo" Text
                                     :> Get '[JSON] Int

type CustomAuthAPI = "something" :> Authorization "Basic" Text
                                 :> Get '[JSON] Int

type CustomHeaderAPI = "something" :> MyLovelyHorse Text
                                   :> Get '[JSON] Int

type CustomHeaderAPI2 = "something" :> WhatsForDinner Text
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
               | Axios
               | AxiosCustom
                 deriving (Show, Eq)

customOptions :: CommonGeneratorOptions
customOptions = defCommonGeneratorOptions
  { successCallback = "okCallback"
  , errorCallback = "errorCallback"
  }

spec :: Spec
spec = describe "Servant.JQuery" $ do
    generateJSSpec Vanilla       JS.generateVanillaJS
    generateJSSpec VanillaCustom (JS.generateVanillaJSWith customOptions)
    generateJSSpec JQuery        JQ.generateJQueryJS
    generateJSSpec JQueryCustom  (JQ.generateJQueryJSWith customOptions)
    generateJSSpec Angular       (NG.generateAngularJS NG.defAngularOptions)
    generateJSSpec AngularCustom (NG.generateAngularJSWith NG.defAngularOptions customOptions)
    generateJSSpec Axios        (AX.generateAxiosJS AX.defAxiosOptions)
    generateJSSpec AxiosCustom  (AX.generateAxiosJSWith (AX.defAxiosOptions { AX.withCredentials = True }) customOptions)

    angularSpec    Angular
    axiosSpec
    --angularSpec    AngularCustom

shouldContain :: Text -> Text -> Expectation
a `shouldContain` b  = shouldSatisfy a (T.isInfixOf b)

shouldNotContain :: Text -> Text -> Expectation
a `shouldNotContain` b  = shouldNotSatisfy a (T.isInfixOf b)

axiosSpec :: Spec
axiosSpec = describe specLabel $ do
    let reqList = listFromAPI (Proxy :: Proxy NoTypes) (Proxy :: Proxy TestAPI)
    it "should add withCredentials when needed" $ do
        let jsText = genJS withCredOpts $ reqList
        output jsText
        jsText `shouldContain` "withCredentials: true"
    it "should add xsrfCookieName when needed" $ do
        let jsText = genJS cookieOpts $ reqList
        output jsText
        jsText `shouldContain` ("xsrfCookieName: 'MyXSRFcookie'")
    it "should add withCredentials when needed" $ do
        let jsText = genJS headerOpts $ reqList
        output jsText
        jsText `shouldContain` ("xsrfHeaderName: 'MyXSRFheader'")
    where
        specLabel = "Axios"
        output _ = return ()
        withCredOpts = AX.defAxiosOptions { AX.withCredentials = True }
        cookieOpts = AX.defAxiosOptions { AX.xsrfCookieName = Just "MyXSRFcookie" }
        headerOpts = AX.defAxiosOptions { AX.xsrfHeaderName = Just "MyXSRFheader" }
        genJS :: AxiosOptions -> [AjaxReq] -> Text
        genJS opts req = mconcat . map (AX.generateAxiosJS opts) $ req

angularSpec :: TestNames -> Spec
angularSpec test = describe specLabel $ do
    let reqList = listFromAPI (Proxy :: Proxy NoTypes) (Proxy :: Proxy TestAPI)
    it "should implement a service globally" $ do
        let jsText = genJS reqList
        output jsText
        jsText `shouldContain` (".service('" <> testName <> "'")

    it "should depend on $http service globally" $ do
        let jsText = genJS reqList
        output jsText
        jsText `shouldContain` ("('" <> testName <> "', function($http) {")

    it "should not depend on $http service in handlers" $ do
        let jsText = genJS reqList
        output jsText
        jsText `shouldNotContain` "getsomething($http, "
    where
        specLabel = "AngularJS(" <> (show test) <> ")"
        output _ = return ()
        testName = "MyService"
        ngOpts = NG.defAngularOptions { NG.serviceName = testName }
        genJS req = NG.angularService ngOpts req

generateJSSpec :: TestNames -> (AjaxReq -> Text) -> Spec
generateJSSpec n gen = describe specLabel $ do
    let parseFromText = parse program ""
    it "should generate valid javascript" $ do
        let s = jsForAPI (Proxy :: Proxy TestAPI) (mconcat . map gen)
        parseFromText s `shouldSatisfy` isRight

    it "should use non-empty function names" $ do
        let (_ :<|> topLevel) = javascript (Proxy :: Proxy TopLevelRawAPI)
        output $ genJS (topLevel "GET")
        parseFromText (genJS $ topLevel "GET") `shouldSatisfy` isRight

    it "should handle simple HTTP headers" $ do
        let jsText = genJS $ javascript headerHandlingProxy
        output jsText
        parseFromText jsText `shouldSatisfy` isRight
        jsText `shouldContain` "headerFoo"
        jsText `shouldContain`  (header n "Foo" $ "headerFoo")

    it "should handle complex HTTP headers" $ do
        let jsText = genJS $ javascript customAuthProxy
        output jsText
        parseFromText jsText `shouldSatisfy` isRight
        jsText `shouldContain` "headerAuthorization"
        jsText `shouldContain`  (header n "Authorization" $ "\"Basic \" + headerAuthorization")

    it "should handle complex, custom HTTP headers" $ do
        let jsText = genJS $ javascript customHeaderProxy
        output jsText
        parseFromText jsText `shouldSatisfy` isRight
        jsText `shouldContain` "headerXMyLovelyHorse"
        jsText `shouldContain`  (header n "X-MyLovelyHorse" $ "\"I am good friends with \" + headerXMyLovelyHorse")

    it "should handle complex, custom HTTP headers (template replacement)" $ do
        let jsText = genJS $ javascript customHeaderProxy2
        output jsText
        parseFromText jsText `shouldSatisfy` isRight
        jsText `shouldContain` "headerXWhatsForDinner"
        jsText `shouldContain`  (header n "X-WhatsForDinner" $ "\"I would like \" + headerXWhatsForDinner + \" with a cherry on top.\"")

    it "can generate the whole javascript code string at once with jsForAPI" $ do
        let jsStr = jsForAPI (Proxy :: Proxy TestAPI) (mconcat . map gen)
        parseFromText jsStr `shouldSatisfy` isRight
    where
        specLabel = "generateJS(" <> (show n) <> ")"
        output _ = return ()
        genJS req = gen req
        header :: TestNames -> Text -> Text -> Text
        header v headerName headerValue
            | v `elem` [Vanilla, VanillaCustom] = "xhr.setRequestHeader(\"" <> headerName <> "\", " <> headerValue <> ");\n"
            | otherwise = "headers: { \"" <> headerName <> "\": " <> headerValue <> " }\n"
