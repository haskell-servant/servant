{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Servant.ForeignSpec where

import           Data.Proxy
import           Servant.Foreign
import           Servant.Test.ComprehensiveAPI
import           Servant.Types.SourceT
                 (SourceT)

import           Test.Hspec


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
      `shouldBe` "getHyphen-atedCounter"

----------------------------------------------------------------------

-- This declaration simply checks that all instances are in place.
_ = listFromAPI (Proxy :: Proxy LangX) (Proxy :: Proxy String) comprehensiveAPIWithoutRaw

----------------------------------------------------------------------

data LangX

instance HasForeignType LangX String NoContent where
  typeFor _ _ _ = "voidX"

instance HasForeignType LangX String (Headers ctyps NoContent) where
  typeFor _ _ _ = "voidX"

instance HasForeignType LangX String Int where
  typeFor _ _ _ = "intX"

instance HasForeignType LangX String (SourceT m a) where
  typeFor _ _ _ = "streamTX"

instance HasForeignType LangX String Bool where
  typeFor _ _ _ = "boolX"

instance {-# OVERLAPPING #-} HasForeignType LangX String String where
  typeFor _ _ _ = "stringX"

instance {-# OVERLAPPABLE #-} HasForeignType LangX String a => HasForeignType LangX String [a] where
  typeFor lang ftype _ = "listX of " <> typeFor lang ftype (Proxy :: Proxy a)

instance (HasForeignType LangX String a) => HasForeignType LangX String (Maybe a) where
  typeFor lang ftype _ = "maybe " <> typeFor lang ftype (Proxy :: Proxy a)

type TestApi
    = "test" :> Header "header" [String] :> QueryFlag "flag" :> Get '[JSON] Int
 :<|> "test" :> QueryParam "param" Int :> ReqBody '[JSON] [String] :> Post '[JSON] NoContent
 :<|> "test" :> QueryParams "params" Int :> ReqBody '[JSON] String :> Put '[JSON] NoContent
 :<|> "test" :> Capture "id" Int :> Delete '[JSON] NoContent
 :<|> "test" :> CaptureAll "ids" Int :> Get '[JSON] [Int]
 :<|> "test" :> EmptyAPI

testApi :: [Req String]
testApi = listFromAPI (Proxy :: Proxy LangX) (Proxy :: Proxy String) (Proxy :: Proxy TestApi)

listFromAPISpec :: Spec
listFromAPISpec = describe "listFromAPI" $ do
  it "generates 5 endpoints for TestApi" $ do
    length testApi `shouldBe` 5

  let [getReq, postReq, putReq, deleteReq, captureAllReq] = testApi

  it "collects all info for get request" $ do
    shouldBe getReq $ defReq
      { _reqUrl        = Url
          [ Segment $ Static "test" ]
          [ QueryArg (Arg "flag" "boolX") Flag ]
          Nothing
      , _reqMethod     = "GET"
      , _reqHeaders    = [HeaderArg $ Arg "header" "maybe listX of stringX"]
      , _reqBody       = Nothing
      , _reqReturnType = Just "intX"
      , _reqFuncName   = FunctionName ["get", "test"]
      }

  it "collects all info for post request" $ do
    shouldBe postReq $ defReq
      { _reqUrl        = Url
          [ Segment $ Static "test" ]
          [ QueryArg (Arg "param" "maybe intX") Normal ]
          Nothing
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
          -- Should this be |intX| or |listX of intX| ?
          [ QueryArg (Arg "params" "listX of intX") List ]
          Nothing
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
          Nothing
      , _reqMethod     = "DELETE"
      , _reqHeaders    = []
      , _reqBody       = Nothing
      , _reqReturnType = Just "voidX"
      , _reqFuncName   = FunctionName ["delete", "test", "by", "id"]
      }

  it "collects all info for capture all request" $ do
    shouldBe captureAllReq $ defReq
      { _reqUrl        = Url
          [ Segment $ Static "test"
          , Segment $ Cap (Arg "ids" "listX of intX") ]
          []
          Nothing
      , _reqMethod     = "GET"
      , _reqHeaders    = []
      , _reqBody       = Nothing
      , _reqReturnType = Just "listX of intX"
      , _reqFuncName   = FunctionName ["get", "test", "by", "ids"]
      }
