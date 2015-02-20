{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.API.ContentTypesSpec where

import           Control.Applicative
import           Data.Aeson
import           Data.Proxy
import qualified Data.Text                as TextS
import qualified Data.Text.Lazy           as TextL
import           GHC.Generics
import           Test.Hspec
import           Test.QuickCheck

import           Servant.API.ContentTypes

spec :: Spec
spec = describe "Servant.API.ContentTypes" $ do

    describe "The JSON Content-Type type" $ do

        it "has fromByteString reverse toByteString for valid top-level json ([Int]) " $ do
            let p = Proxy :: Proxy JSON
            property $ \x -> fromByteString p (toByteString p x) == Right (x::[Int])

        it "has fromByteString reverse toByteString for valid top-level json " $ do
            let p = Proxy :: Proxy JSON
            property $ \x -> fromByteString p (toByteString p x) == Right (x::SomeData)

    describe "The PlainText Content-Type type" $ do

        it "has fromByteString reverse toByteString (lazy Text)" $ do
            let p = Proxy :: Proxy PlainText
            property $ \x -> fromByteString p (toByteString p x) == Right (x::TextL.Text)

        it "has fromByteString reverse toByteString (strict Text)" $ do
            let p = Proxy :: Proxy PlainText
            property $ \x -> fromByteString p (toByteString p x) == Right (x::TextS.Text)


data SomeData = SomeData { record1 :: String, record2 :: Int }
    deriving (Generic, Eq, Show)

instance FromJSON SomeData
instance ToJSON SomeData
instance Arbitrary SomeData where
    arbitrary = SomeData <$> arbitrary <*> arbitrary

instance Arbitrary TextL.Text where
    arbitrary = TextL.pack <$> arbitrary

instance Arbitrary TextS.Text where
    arbitrary = TextS.pack <$> arbitrary
