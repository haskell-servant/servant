{-# LANGUAGE CPP               #-}
#include "overlapping-compat.h"

module Servant.ForeignSpec where

import Data.Monoid ((<>))
import Data.Proxy
import Servant.Foreign

import Test.Hspec

spec :: Spec
spec = describe "Servant.Foreign" $ do
  camelCaseSpec
  listFromAPISpec

camelCaseSpec :: Spec
camelCaseSpec = describe "camelCase" $ do
  it "converts FunctionNames to camelCase" $ do
    camelCase (FunctionName ["post", "counter", "inc"])
      `shouldBe` "postCounterInc"
    camelCase (FunctionName ["get", "hyphen-ated", "counter"])
      `shouldBe` "getHyphenatedCounter"

----------------------------------------------------------------------

data LangX

instance HasForeignType LangX String () where
  typeFor _ _ _ = "voidX"

instance HasForeignType LangX String Int where
  typeFor _ _ _ = "intX"

instance HasForeignType LangX String Bool where
  typeFor _ _ _ = "boolX"

instance OVERLAPPING_ HasForeignType LangX String String where
  typeFor _ _ _ = "stringX"

instance OVERLAPPABLE_ HasForeignType LangX String a => HasForeignType LangX String [a] where
  typeFor lang ftype _ = "listX of " <> typeFor lang ftype (Proxy :: Proxy a)

type TestApi
    = "test" :> Header "header" [String] :> QueryFlag "flag" :> Get '[JSON] Int
 :<|> "test" :> QueryParam "param" Int :> ReqBody '[JSON] [String] :> Post '[JSON] ()
 :<|> "test" :> QueryParams "params" Int :> ReqBody '[JSON] String :> Put '[JSON] ()
 :<|> "test" :> Capture "id" Int :> Delete '[JSON] ()

testApi :: [Req String]
testApi = listFromAPI (Proxy :: Proxy LangX) (Proxy :: Proxy String) (Proxy :: Proxy TestApi)

listFromAPISpec :: Spec
listFromAPISpec = describe "listFromAPI" $ do
  it "generates 4 endpoints for TestApi" $ do
    length testApi `shouldBe` 4

  let [getReq, postReq, putReq, deleteReq] = testApi

  it "collects all info for get request" $ do
    shouldBe getReq $ defReq
      { _reqUrl        = Url
          [ Segment $ Static "test" ]
          [ QueryArg (Arg "flag" "boolX") Flag ]
      , _reqMethod     = "GET"
      , _reqHeaders    = [HeaderArg $ Arg "header" "listX of stringX"]
      , _reqBody       = Nothing
      , _reqReturnType = Just "intX"
      , _reqFuncName   = FunctionName ["get", "test"]
      }

  it "collects all info for post request" $ do
    shouldBe postReq $ defReq
      { _reqUrl        = Url
          [ Segment $ Static "test" ]
          [ QueryArg (Arg "param" "intX") Normal ]
      , _reqMethod     = "POST"
      , _reqHeaders    = []
      , _reqBody       = Just "listX of stringX"
      , _reqReturnType = Just "voidX"
      , _reqFuncName   = FunctionName ["post", "test"]
      }

  it "collects all info for put request" $ do
    shouldBe putReq $ defReq
      { _reqUrl        = Url
          [ Segment $ Static "test" ]
          -- Shoud this be |intX| or |listX of intX| ?
          [ QueryArg (Arg "params" "listX of intX") List ]
      , _reqMethod     = "PUT"
      , _reqHeaders    = []
      , _reqBody       = Just "stringX"
      , _reqReturnType = Just "voidX"
      , _reqFuncName   = FunctionName ["put", "test"]
      }

  it "collects all info for delete request" $ do
    shouldBe deleteReq $ defReq
      { _reqUrl        = Url
          [ Segment $ Static "test"
          , Segment $ Cap (Arg "id" "intX") ]
          []
      , _reqMethod     = "DELETE"
      , _reqHeaders    = []
      , _reqBody       = Nothing
      , _reqReturnType = Just "voidX"
      , _reqFuncName   = FunctionName ["delete", "test", "by", "id"]
      }
