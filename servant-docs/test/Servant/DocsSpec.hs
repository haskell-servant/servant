{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# OPTIONS_GHC -freduction-depth=100 #-}

module Servant.DocsSpec where

import           Control.Lens
                 ((&), (<>~))
import           Control.Monad
                 (unless)
import           Control.Monad.Trans.Writer
                 (Writer, runWriter, tell)
import           Data.Aeson
import           Data.List
                 (isInfixOf)
import           Data.Proxy
import           Data.String.Conversions
                 (cs)
import           GHC.Generics
import           Prelude ()
import           Prelude.Compat
import           Test.Tasty
                 (TestName, TestTree, testGroup)
import           Test.Tasty.Golden
                 (goldenVsString)
import           Test.Tasty.HUnit
                 (Assertion, HasCallStack, assertFailure, testCase, (@?=))

import           Servant.API
import           Servant.Docs.Internal
import           Servant.Test.ComprehensiveAPI

-- * comprehensive api

-- This declaration simply checks that all instances are in place.
comprehensiveDocs :: API
comprehensiveDocs = docs comprehensiveAPI

instance ToParam (QueryParam' mods "foo" Int) where
  toParam _ = DocQueryParam "foo" ["1","2","3"] "QueryParams Int" Normal
instance ToParam (QueryParam' mods "bar" Int) where
  toParam _ = DocQueryParam "bar" ["1","2","3"] "QueryParams Int" Normal
instance ToParam (QueryParams "foo" Int) where
  toParam _ = DocQueryParam "foo" ["1","2","3"] "QueryParams Int" List
instance ToParam (QueryFlag "foo") where
  toParam _ = DocQueryParam "foo" [] "QueryFlag" Flag
instance ToCapture (Capture "foo" Int) where
  toCapture _ = DocCapture "foo" "Capture foo Int"
instance ToCapture (CaptureAll "foo" Int) where
  toCapture _ = DocCapture "foo" "Capture all foo Int"
instance ToFragment (Fragment Int) where
  toFragment _ = DocFragment "foo" "Fragment Int"

-- * specs

spec :: TestTree
spec = describe "Servant.Docs" $ do
  golden "comprehensive API" "golden/comprehensive.md" (markdown comprehensiveDocs)

  describe "markdown" $ do
    let md1 = markdown (docs (Proxy :: Proxy TestApi1))
    tests1 md1
    let md2 = markdown (docs (Proxy :: Proxy TestApi2))
    tests2 md2

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
    tests1 md
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
   tests1 md = do
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

    it "contains response samples - dt1field1" $
      md `shouldContain` "\"dt1field1\":\"field 1\""
    it "contains response samples - dt1field2" $
      md `shouldContain` "\"dt1field2\":13"
    it "contains request body samples" $
      md `shouldContain` "17"

    it "does not generate any docs mentioning the 'empty-api' path" $
      md `shouldNotContain` "empty-api"

   tests2 md = do
    it "mentions the content-types from both copies of the route" $ do
      md `shouldContain` "application/json"
      md `shouldContain` "text/plain;charset=utf-8"


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

type TestApi2 = "duplicate-endpoint" :> Get '[JSON]      Datatype1
           :<|> "duplicate-endpoint" :> Get '[PlainText] Int


data TT = TT1 | TT2 deriving (Show, Eq)
data UT = UT1 | UT2 deriving (Show, Eq)

instance ToSample TT where
  toSamples _ = [("eins", TT1), ("zwei", TT2)]

instance ToSample UT where
  toSamples _ = [("yks", UT1), ("kaks", UT2)]

-------------------------------------------------------------------------------
-- HSpec like DSL for tasty
-------------------------------------------------------------------------------

newtype TestTreeM a = TestTreeM (Writer [TestTree] a)
  deriving (Functor, Applicative, Monad)

runTestTreeM :: TestTreeM () -> [TestTree]
runTestTreeM (TestTreeM m) = snd (runWriter m)

class Describe r where
    describe :: TestName -> TestTreeM () -> r

instance a ~ () =>  Describe (TestTreeM a) where
    describe n t = TestTreeM $ tell [ describe n t ]

instance Describe TestTree where
    describe n t = testGroup n $ runTestTreeM t

it :: TestName -> Assertion -> TestTreeM ()
it n assertion = TestTreeM $ tell [ testCase n assertion ]

shouldBe :: (Eq a, Show a, HasCallStack) => a -> a -> Assertion
shouldBe = (@?=)

shouldContain :: (Eq a, Show a, HasCallStack) => [a] -> [a] -> Assertion
shouldContain = compareWith (flip isInfixOf) "does not contain"

shouldNotContain :: (Eq a, Show a, HasCallStack) => [a] -> [a] -> Assertion
shouldNotContain = compareWith (\x y -> not (isInfixOf y x)) "contains"

compareWith :: (Show a, Show b, HasCallStack) => (a -> b -> Bool) -> String -> a -> b -> Assertion
compareWith f msg x y = unless (f x y) $ assertFailure $
    show x ++ " " ++ msg ++ " " ++ show y

golden :: TestName -> FilePath -> String -> TestTreeM ()
golden n fp contents = TestTreeM $ tell
    [ goldenVsString n fp (return (cs contents)) ]
