{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Servant.DocsSpec where

import           Data.Aeson
import           Data.Proxy
import           Data.String.Conversions (cs)
import           GHC.Generics
import           Test.Hspec

import           Servant.API
import           Servant.Docs.Internal

spec :: Spec
spec = describe "Servant.Docs" $ do

  describe "markdown" $ do
    let md = markdown (docs (Proxy :: Proxy TestApi1))

    it "mentions supported content-types" $ do
      md `shouldContain` "application/json"
      md `shouldContain` "text/plain;charset=utf-8"

    it "mentions status codes" $ do
      md `shouldContain` "Status code 200"
      md `shouldContain` "Status code 201"

    it "mentions methods" $ do
      md `shouldContain` "POST"
      md `shouldContain` "GET"

    it "contains response samples" $ do
      md `shouldContain` "{\"dt1field1\":\"field 1\",\"dt1field2\":13}"
    it "contains request body samples" $ do
      md `shouldContain` "17"
-- * APIs

data Datatype1 = Datatype1 { dt1field1 :: String
                           , dt1field2 :: Int
                           } deriving (Eq, Show, Generic)

instance ToJSON Datatype1

instance ToSample Datatype1 where
  toSample = Just $ Datatype1 "field 1" 13

instance ToSample String where
  toSample = Just "a string"

instance ToSample Int where
  toSample = Just 17

instance MimeRender PlainText Int where
  toByteString _ = cs . show


type TestApi1 = Get '[JSON, PlainText] Int
           :<|> ReqBody '[JSON] String :> Post '[JSON] Datatype1

