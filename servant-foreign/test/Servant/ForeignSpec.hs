{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE CPP                   #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances  #-}
#endif

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
camelCaseSpec = describe "camelCase" $
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
instance {-# Overlapping #-} HasForeignType LangX String where
    typeFor _ _ = "stringX"
instance {-# Overlappable #-} HasForeignType LangX a => HasForeignType LangX [a] where
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
    it "generates 4 endpoints for TestApi" $
        length testApi `shouldBe` 4

    let [getReq, postReq, putReq, deleteReq] = testApi

    let reqEq req1 req2 = do
            _reqUrl req1 `shouldBe` _reqUrl  req2
            _reqMethod req1 `shouldBe` _reqMethod req2
            _reqBody req1 `shouldBe` _reqBody req2
            _reqReturnType req1 `shouldBe` _reqReturnType req2
            _funcName req1 `shouldBe` _funcName req2

            let h = case (_reqHeaders req1, _reqHeaders req2) of
                    ([], []) -> True
                    ([HeaderArg a], [HeaderArg b]) -> a == b
                    _ -> False

            h `shouldBe` True

    it "collects all info for get request" $ do
        let req1 = getReq
            req2 = defReq 
                { _reqUrl        = Url
                    [ Segment $ Static "test" ]
                    [ QueryArg ("flag", "boolX") Flag ]
                , _reqMethod     = "GET"
                , _reqHeaders    = [HeaderArg ("header", "listX of stringX")]
                , _reqBody       = Nothing
                , _reqReturnType = "intX"
                , _funcName      = ["get", "test"]
                }

        reqEq req1 req2


    it "collects all info for post request" $ do
        let req1 = getReq
            req2 = defReq
                    { _reqUrl        = Url
                        [ Segment $ Static "test" ]
                        [ QueryArg ("param", "intX") Normal ]
                    , _reqMethod     = "POST"
                    , _reqHeaders    = []
                    , _reqBody       = Just "listX of stringX"
                    , _reqReturnType = "voidX"
                    , _funcName      = ["post", "test"]
                    }

        reqEq req1 req2

    it "collects all info for put request" $ do
        let req1 = getReq
            req2 = defReq
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

        reqEq req1 req2

    it "collects all info for delete request" $ do
        let req1 = getReq
            req2 = defReq
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

        reqEq req1 req2
