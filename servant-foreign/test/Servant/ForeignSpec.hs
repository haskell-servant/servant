{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE CPP                   #-}

#include "overlapping-compat.h"

module Servant.ForeignSpec where

import Data.Monoid ((<>))
import Data.Proxy
import Servant.Foreign
import Servant.Foreign.Internal

import Test.Hspec

spec :: Spec
spec = describe "Servant.Foreign" $ do
  camelCaseSpec
  listFromAPISpec

camelCaseSpec :: Spec
camelCaseSpec = describe "camelCase" $ do
  it "converts FunctionNames to camelCase" $ do
    camelCase ["post", "counter", "inc"] `shouldBe` "postCounterInc"
    camelCase ["get", "hyphen-ated", "counter"] `shouldBe` "getHyphenatedCounter"

----------------------------------------------------------------------

data LangX

instance HasForeignType LangX () where
    typeFor _ _ = "voidX"
instance HasForeignType LangX Int where
    typeFor _ _ = "intX"
instance HasForeignType LangX Bool where
    typeFor _ _ = "boolX"
instance OVERLAPPING_ HasForeignType LangX String where
    typeFor _ _ = "stringX"
instance OVERLAPPABLE_ HasForeignType LangX a => HasForeignType LangX [a] where
    typeFor lang _ = "listX of " <> typeFor lang (Proxy :: Proxy a)

type TestApi
    = "test" :> Header "header" [String] :> QueryFlag "flag" :> Get '[JSON] Int
 :<|> "test" :> QueryParam "param" Int :> ReqBody '[JSON] [String] :> Post '[JSON] ()
 :<|> "test" :> QueryParams "params" Int :> ReqBody '[JSON] String :> Put '[JSON] ()
 :<|> "test" :> Capture "id" Int :> Delete '[JSON] ()

testApi :: [Req]
testApi = listFromAPI (Proxy :: Proxy LangX) (Proxy :: Proxy TestApi)

listFromAPISpec :: Spec
listFromAPISpec = describe "listFromAPI" $ do
    it "generates 4 endpoints for TestApi" $ do
        length testApi `shouldBe` 4

    let [getReq, postReq, putReq, deleteReq] = testApi

    it "collects all info for get request" $ do
        shouldBe getReq $ defReq
            { _reqUrl        = Url
                [ Segment $ Static "test" ]
                [ QueryArg ("flag", "boolX") Flag ]
            , _reqMethod     = "GET"
            , _reqHeaders    = [HeaderArg ("header", "listX of stringX")]
            , _reqBody       = Nothing
            , _reqReturnType = "intX"
            , _funcName      = ["get", "test"]
            }

    it "collects all info for post request" $ do
        shouldBe postReq $ defReq
            { _reqUrl        = Url
                [ Segment $ Static "test" ]
                [ QueryArg ("param", "intX") Normal ]
            , _reqMethod     = "POST"
            , _reqHeaders    = []
            , _reqBody       = Just "listX of stringX"
            , _reqReturnType = "voidX"
            , _funcName      = ["post", "test"]
            }

    it "collects all info for put request" $ do
        shouldBe putReq $ defReq
            { _reqUrl        = Url
                [ Segment $ Static "test" ]
                -- Shoud this be |intX| or |listX of intX| ?
                [ QueryArg ("params", "listX of intX") List ]
            , _reqMethod     = "PUT"
            , _reqHeaders    = []
            , _reqBody       = Just "stringX"
            , _reqReturnType = "voidX"
            , _funcName      = ["put", "test"]
            }

    it "collects all info for delete request" $ do
        shouldBe deleteReq $ defReq
            { _reqUrl        = Url
                [ Segment $ Static "test"
                , Segment $ Cap ("id", "intX") ]
                []
            , _reqMethod     = "DELETE"
            , _reqHeaders    = []
            , _reqBody       = Nothing
            , _reqReturnType = "voidX"
            , _funcName      = ["delete", "test", "by", "id"]
            }

