{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.API.ContentTypesSpec where

import           Control.Applicative
import           Control.Arrow
import           Data.Aeson
import           Data.Aeson.Parser          (jstring)
import           Data.Attoparsec.ByteString (parseOnly)
import           Data.Function              (on)
import           Data.Proxy

import           Data.ByteString.Char8      (ByteString, append, pack)
import qualified Data.ByteString.Lazy       as BSL
import           Data.List                  (maximumBy)
import           Data.Maybe                 (fromJust, isJust, isNothing)
import           Data.String                (IsString (..))
import           Data.String.Conversions    (cs)
import qualified Data.Text                  as TextS
import qualified Data.Text.Lazy             as TextL
import           GHC.Generics
import qualified Network.HTTP.Types         as H
import           Network.URL                (exportParams, importParams)
import           Test.Hspec
import           Test.HUnit
import           Test.QuickCheck
import           Test.QuickCheck.Instances  ()

import           Servant.API.ContentTypes

spec :: Spec
spec = describe "Servant.API.ContentTypes" $ do

    describe "The JSON Content-Type type" $ do

        it "has fromByteString reverse toByteString for valid top-level json ([Int]) " $ do
            let p = Proxy :: Proxy JSON
            property $ \x -> uncurry (fromByteString p) (toByteString p x) == Right (x::[Int])

        it "has fromByteString reverse toByteString for valid top-level json " $ do
            let p = Proxy :: Proxy JSON
            property $ \x -> uncurry (fromByteString p) (toByteString p x) == Right (x::SomeData)

    describe "The FormUrlEncoded Content-Type type" $ do

        let isNonNull ("", "") = False
            isNonNull _        = True

        it "has fromByteString reverse toByteString" $ do
            let p = Proxy :: Proxy FormUrlEncoded
            property $ \x -> all isNonNull x
                ==> uncurry (fromByteString p) (toByteString p x) == Right (x::[(TextS.Text,TextS.Text)])

        it "has fromByteString reverse exportParams (Network.URL)" $ do
            let p = Proxy :: Proxy FormUrlEncoded
            property $ \x -> all isNonNull x
                ==> (fromByteString p [] . cs . exportParams . map (cs *** cs) $ x) == Right (x::[(TextS.Text,TextS.Text)])

        it "has importParams (Network.URL) reverse toByteString" $ do
            let p = Proxy :: Proxy FormUrlEncoded
            property $ \x -> all isNonNull x
                ==> (fmap (map (cs *** cs)) . importParams . cs . snd . toByteString p $ x) == Just (x::[(TextS.Text,TextS.Text)])

    describe "The PlainText Content-Type type" $ do

        it "has fromByteString reverse toByteString (lazy Text)" $ do
            let p = Proxy :: Proxy PlainText
            property $ \x -> uncurry (fromByteString p) (toByteString p x) == Right (x::TextL.Text)

        it "has fromByteString reverse toByteString (strict Text)" $ do
            let p = Proxy :: Proxy PlainText
            property $ \x -> uncurry (fromByteString p) (toByteString p x) == Right (x::TextS.Text)

    describe "The OctetStream Content-Type type" $ do

        it "is id (Lazy ByteString)" $ do
            let p = Proxy :: Proxy OctetStream
            property $ \x -> toByteString p x == ([], x :: BSL.ByteString)
                && fromByteString p [] x == Right x

        it "is fromStrict/toStrict (Strict ByteString)" $ do
            let p = Proxy :: Proxy OctetStream
            property $ \x -> toByteString p x == ([], BSL.fromStrict x)
                && fromByteString p [] (BSL.fromStrict x) == Right x

    describe "handleAcceptH" $ do

        it "returns Nothing if the 'Accept' header doesn't match" $ do
            handleAcceptH (Proxy :: Proxy '[JSON]) "text/plain" (3 :: Int)
                `shouldSatisfy` isNothing

        it "returns Just if the 'Accept' header matches" $ do
            handleAcceptH (Proxy :: Proxy '[JSON]) "*/*" (3 :: Int)
                `shouldSatisfy` isJust
            handleAcceptH (Proxy :: Proxy '[PlainText, JSON]) "application/json" (3 :: Int)
                `shouldSatisfy` isJust
            handleAcceptH (Proxy :: Proxy '[PlainText, JSON, OctetStream])
                "application/octet-stream" ("content" :: ByteString)
                `shouldSatisfy` isJust

        it "returns the Content-Type as the first element of the tuple" $ do
            handleAcceptH (Proxy :: Proxy '[JSON]) "*/*" (3 :: Int)
                `shouldSatisfy` ((== "application/json") . fst . fromJust)
            handleAcceptH (Proxy :: Proxy '[PlainText, JSON]) "application/json" (3 :: Int)
                `shouldSatisfy` ((== "application/json") . fst . fromJust)
            handleAcceptH (Proxy :: Proxy '[PlainText, JSON, OctetStream])
                "application/octet-stream" ("content" :: ByteString)
                `shouldSatisfy` ((== "application/octet-stream") . fst . fromJust)

        it "returns the appropriately serialized representation" $ do
            property $ \x -> handleAcceptH (Proxy :: Proxy '[JSON]) "*/*" (x :: SomeData)
                == Just ("application/json", ([], encode x))

        it "respects the Accept spec ordering" $ do
            let highest a b c = maximumBy (compare `on` snd)
                        [ ("application/octet-stream", a)
                        , ("application/json", b)
                        , ("text/plain;charset=utf-8", c)
                        ]
            let acceptH a b c = addToAccept (Proxy :: Proxy OctetStream) a $
                                    addToAccept (Proxy :: Proxy JSON) b $
                                    addToAccept (Proxy :: Proxy PlainText ) c ""
            let val a b c i = handleAcceptH (Proxy :: Proxy '[OctetStream, JSON, PlainText])
                                            (acceptH a b c) (i :: Int)
            property $ \a b c i -> fst (fromJust $ val a b c i) == fst (highest a b c)

    describe "handleCTypeH" $ do

        it "returns Nothing if the 'Content-Type' header doesn't match" $ do
            handleCTypeH (Proxy :: Proxy '[JSON]) [] "text/plain" "𝓽𝓱𝓮 𝓽𝓲𝓶𝓮 𝓱𝓪𝓼 𝓬𝓸𝓶𝓮, 𝓽𝓱𝓮 𝔀𝓪𝓵𝓻𝓾𝓼 𝓼𝓪𝓲𝓭 "
                `shouldBe` (Nothing :: Maybe (Either String Value))

        context "the 'Content-Type' header matches" $ do
            it "returns Just if the parameter matches" $ do
                handleCTypeH (Proxy :: Proxy '[JSON]) [] "application/json"
                    "𝕥𝕠 𝕥𝕒𝕝𝕜 𝕠𝕗 𝕞𝕒𝕟𝕪 𝕥𝕙𝕚𝕟𝕘𝕤 "
                    `shouldSatisfy` (isJust :: Maybe (Either String Value) -> Bool)

            it "returns Just if there is no parameter" $ do
                handleCTypeH (Proxy :: Proxy '[JSON]) [] "application/json"
                    "𝕥𝕠 𝕥𝕒𝕝𝕜 𝕠𝕗 𝕞𝕒𝕟𝕪 𝕥𝕙𝕚𝕟𝕘𝕤 "
                    `shouldSatisfy` (isJust :: Maybe (Either String Value) -> Bool)

            it "returns Just Left if the decoding fails" $ do
                let isJustLeft :: Maybe (Either String Value) -> Bool
                    isJustLeft (Just (Left _)) = True
                    isJustLeft _ = False
                handleCTypeH (Proxy :: Proxy '[JSON]) [] "application/json"
                    "𝕺𝖋 𝖘𝖍𝖔𝖊𝖘--𝖆𝖓𝖉 𝖘𝖍𝖎𝖕𝖘--𝖆𝖓𝖉 𝖘𝖊𝖆𝖑𝖎𝖓𝖌-𝖜𝖆𝖝-- "
                    `shouldSatisfy` isJustLeft

            it "returns Just (Right val) if the decoding succeeds" $ do
                let val = SomeData "Of cabbages--and kings" 12
                handleCTypeH (Proxy :: Proxy '[JSON]) [] "application/json"
                    (encode val)
                    `shouldBe` Just (Right val)

    describe "eitherDecodeLenient" $ do

        it "parses top-level strings" $ do
            let toMaybe = either (const Nothing) Just
            -- The Left messages differ, so convert to Maybe
            property $ \x -> toMaybe (eitherDecodeLenient x)
                `shouldBe` toMaybe (parseOnly jstring $ cs x)

    describe "ResponseHeaders" $ do

        it "decoding succeeds and only returns the specified header" $ do
            let res = fromByteString (Proxy :: Proxy (ResponseHeaders '["TestHeader"] JSON))
                                     [("Foo", "NOOOOO"), ("TestHeader", "YAY")]
                                     "1"
            res `shouldBe` Right ([("TestHeader", "YAY") :: H.Header], (1 :: Int))

        it "decoding fails if a required header is not present" $ do
            let res = fromByteString (Proxy :: Proxy (ResponseHeaders '["TestHeader"] JSON))
                                     [("Foo", "NOOOOO")]
                                     "1" :: Either String ([H.Header], Int)
            case res of
                Right _ -> assertFailure "Expected an error"
                Left _ -> return ()


data SomeData = SomeData { record1 :: String, record2 :: Int }
    deriving (Generic, Eq, Show)

newtype ZeroToOne = ZeroToOne Float
    deriving (Eq, Show, Ord)

instance FromJSON SomeData

instance ToJSON SomeData

instance Arbitrary SomeData where
    arbitrary = SomeData <$> arbitrary <*> arbitrary

instance Arbitrary ZeroToOne where
    arbitrary = ZeroToOne <$> elements [ x / 10 | x <- [1..10]]

instance MimeRender OctetStream Int where
    toByteString _ = ([],) . cs . show

instance MimeRender PlainText Int where
    toByteString _ = ([],) . cs . show

instance MimeRender PlainText ByteString where
    toByteString _ = ([],) . cs

instance ToJSON ByteString where
    toJSON x = object [ "val" .= x ]

instance IsString AcceptHeader where
    fromString = AcceptHeader . fromString

addToAccept :: Accept a => Proxy a -> ZeroToOne -> AcceptHeader -> AcceptHeader
addToAccept p (ZeroToOne f) (AcceptHeader h) = AcceptHeader (cont h)
    where new = cs (show $ contentType p) `append` "; q=" `append` pack (show f)
          cont "" = new
          cont old = old `append` ", " `append` new
