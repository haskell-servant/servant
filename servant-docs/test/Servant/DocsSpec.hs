{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -freduction-depth=100 #-}
#else
{-# OPTIONS_GHC -fcontext-stack=100 #-}
#endif

module Servant.DocsSpec where

import           Control.Lens
import           Data.Aeson
import           Data.Monoid
import           Data.Proxy
import           Data.String.Conversions (cs)
import           GHC.Generics
import           Test.Hspec

import           Servant.API
import           Servant.API.Internal.Test.ComprehensiveAPI
import           Servant.Docs.Internal

-- * comprehensive api

-- This declaration simply checks that all instances are in place.
_ = docs comprehensiveAPI

instance ToParam (QueryParam' mods "foo" Int) where
  toParam = error "unused"
instance ToParam (QueryParam' mods "bar" Int) where
  toParam = error "unused"
instance ToParam (QueryParams "foo" Int) where
  toParam = error "unused"
instance ToParam (QueryFlag "foo") where
  toParam = error "unused"
instance ToCapture (Capture "foo" Int) where
  toCapture = error "unused"
instance ToCapture (CaptureAll "foo" Int) where
  toCapture = error "unused"

-- * specs

spec :: Spec
spec = describe "Servant.Docs" $ do

  describe "markdown" $ do
    let md = markdown (docs (Proxy :: Proxy TestApi1))
    tests md

  describe "markdown with extra info" $ do
    let
      extra = extraInfo
              (Proxy :: Proxy (Get '[JSON, PlainText] (Headers '[Header "Location" String] Int)))
              (defAction & notes <>~ [DocNote "Get an Integer" ["get an integer in Json or plain text"]])
              <>
              extraInfo
              (Proxy :: Proxy (ReqBody '[JSON] String :> Post '[JSON] Datatype1))
              (defAction & notes <>~ [DocNote "Post data" ["Posts some Json data"]])
      md = markdown (docsWith defaultDocOptions [] extra (Proxy :: Proxy TestApi1))
    tests md
    it "contains the extra info provided" $ do
      md `shouldContain` "Get an Integer"
      md `shouldContain` "Post data"
      md `shouldContain` "get an integer in Json or plain text"
      md `shouldContain` "Posts some Json data"

  describe "tuple samples" $ do
    it "looks like expected" $ do
      (toSample  (Proxy :: Proxy (TT, UT)))     `shouldBe` Just (TT1,UT1)
      (toSample  (Proxy :: Proxy (TT, UT, UT))) `shouldBe` Just (TT1,UT1,UT1)
      (toSamples (Proxy :: Proxy (TT, UT)))     `shouldBe`
         [ ("eins, yks",(TT1,UT1)), ("eins, kaks",(TT1,UT2))
         , ("zwei, yks",(TT2,UT1)), ("zwei, kaks",(TT2,UT2))
         ]
      (toSamples (Proxy :: Proxy (TT, UT, UT))) `shouldBe`
         [ ("eins, yks, yks",(TT1,UT1,UT1))
         , ("eins, yks, kaks",(TT1,UT1,UT2))
         , ("zwei, yks, yks",(TT2,UT1,UT1))
         , ("eins, kaks, yks",(TT1,UT2,UT1))
         , ("zwei, yks, kaks",(TT2,UT1,UT2))
         , ("eins, kaks, kaks",(TT1,UT2,UT2))
         , ("zwei, kaks, yks",(TT2,UT2,UT1))
         , ("zwei, kaks, kaks",(TT2,UT2,UT2))
         ]


 where
   tests md = do
    it "mentions supported content-types" $ do
      md `shouldContain` "application/json"
      md `shouldContain` "text/plain;charset=utf-8"

    it "mentions status codes" $ do
      md `shouldContain` "Status code 200"

    it "has methods as section headers" $ do
      md `shouldContain` "## POST"
      md `shouldContain` "## GET"

    it "mentions headers" $ do
      md `shouldContain` "- This endpoint is sensitive to the value of the **X-Test** HTTP header."

    it "contains response samples" $
      md `shouldContain` "{\"dt1field1\":\"field 1\",\"dt1field2\":13}"
    it "contains request body samples" $
      md `shouldContain` "17"

    it "does not generate any docs mentioning the 'empty-api' path" $
      md `shouldNotContain` "empty-api"


-- * APIs

data Datatype1 = Datatype1 { dt1field1 :: String
                           , dt1field2 :: Int
                           } deriving (Eq, Show, Generic)

instance ToJSON Datatype1

instance ToSample Datatype1 where
  toSamples _ = singleSample $ Datatype1 "field 1" 13

instance ToSample Char where
  toSamples _ = samples ['a'..'z']

instance ToSample Int where
  toSamples _ = singleSample 17

instance MimeRender PlainText Int where
  mimeRender _ = cs . show

type TestApi1 = Get '[JSON, PlainText] (Headers '[Header "Location" String] Int)
           :<|> ReqBody '[JSON] String :> Post '[JSON] Datatype1
           :<|> Header "X-Test" Int :> Put '[JSON] Int
           :<|> "empty-api" :> EmptyAPI

data TT = TT1 | TT2 deriving (Show, Eq)
data UT = UT1 | UT2 deriving (Show, Eq)

instance ToSample TT where
  toSamples _ = [("eins", TT1), ("zwei", TT2)]

instance ToSample UT where
  toSamples _ = [("yks", UT1), ("kaks", UT2)]
