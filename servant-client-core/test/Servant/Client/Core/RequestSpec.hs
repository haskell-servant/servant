{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Servant.Client.Core.RequestSpec (spec) where


import           Prelude ()
import           Prelude.Compat
import           Control.Monad
import           Data.List (isInfixOf)
import           Servant.Client.Core.Request
import           Test.Hspec

newtype DataWithRequest = DataWithRequest (RequestF RequestBody ())
  deriving Show

spec :: Spec
spec = do
  describe "Request" $ do
    describe "show" $ do
      it "has parenthesis correctly positioned" $ do
        let d = DataWithRequest (void defaultRequest)
        show d `shouldBe` "DataWithRequest (Request {requestPath = ()\
                                                  \, requestQueryString = fromList []\
                                                  \, requestBody = Nothing\
                                                  \, requestAccept = fromList []\
                                                  \, requestHeaders = fromList []\
                                                  \, requestHttpVersion = HTTP/1.1\
                                                  \, requestMethod = \"GET\"})"
      it "redacts the authorization header" $ do
        let request = void $ defaultRequest { requestHeaders = pure ("authorization", "secret") }
        isInfixOf "secret" (show request) `shouldBe` False
