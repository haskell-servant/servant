{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE PolyKinds             #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.API.ContentTypesSpec where

import           Prelude ()
import           Prelude.Compat

import           Data.Aeson
                 (FromJSON, ToJSON (..), Value, decode, encode, object, (.=))
import           Data.ByteString.Char8
                 (ByteString, append, pack)
import qualified Data.ByteString.Lazy                             as BSL
import qualified Data.ByteString.Lazy.Char8                       as BSL8
import           Data.Either
import           Data.Function
                 (on)
import           Data.List
                 (sortBy)
import qualified Data.List.NonEmpty                               as NE
import           Data.Maybe
                 (fromJust, isJust, isNothing)
import           Data.Proxy
import           Data.String
                 (IsString (..))
import           Data.String.Conversions
                 (cs)
import qualified Data.Text                                        as TextS
import qualified Data.Text.Encoding                               as TextSE
import qualified Data.Text.Lazy                                   as TextL
import           GHC.Generics
import           Test.Hspec
import           Test.QuickCheck
import           "quickcheck-instances" Test.QuickCheck.Instances ()
import           Text.Read
                 (readMaybe)

import           Servant.API.ContentTypes

spec :: Spec
spec = describe "Servant.API.ContentTypes" $ do

    describe "handleAcceptH" $ do
        let p = Proxy :: Proxy '[PlainText]

        it "matches any charset if none were provided" $ do
            let without = handleAcceptH p (AcceptHeader "text/plain")
                with    = handleAcceptH p (AcceptHeader "text/plain;charset=utf-8")
                wisdom  = "ubi sub ubi" :: String
            without wisdom `shouldBe` with wisdom

        it "does not match non utf-8 charsets" $  do
            let badCharset = handleAcceptH p (AcceptHeader "text/plain;charset=whoknows")
                s          = "cheese" :: String
            badCharset s `shouldBe` Nothing

    describe "The JSON Content-Type type" $ do
        let p = Proxy :: Proxy JSON

        it "handles whitespace at end of input" $ do
            mimeUnrender p "[1] " `shouldBe` Right [1 :: Int]

        it "handles whitespace at beginning of input" $ do
            mimeUnrender p " [1] " `shouldBe` Right [1 :: Int]

        it "does not like junk at end of input" $ do
            mimeUnrender p "[1] this probably shouldn't work"
              `shouldSatisfy` (isLeft :: Either a [Int] -> Bool)

        it "has mimeUnrender reverse mimeRender for valid top-level json ([Int]) " $ do
            property $ \x -> mimeUnrender p (mimeRender p x) == Right (x::[Int])

        it "has mimeUnrender reverse mimeRender for valid top-level json " $ do
            property $ \x -> mimeUnrender p (mimeRender p x) == Right (x::SomeData)

    describe "The PlainText Content-Type type" $ do
        let p = Proxy :: Proxy PlainText

        it "has mimeUnrender reverse mimeRender (lazy Text)" $ do
            property $ \x -> mimeUnrender p (mimeRender p x) == Right (x::TextL.Text)

        it "has mimeUnrender reverse mimeRender (strict Text)" $ do
            property $ \x -> mimeUnrender p (mimeRender p x) == Right (x::TextS.Text)

    describe "The OctetStream Content-Type type" $ do
        let p = Proxy :: Proxy OctetStream

        it "is id (Lazy ByteString)" $ do
            property $ \x -> mimeRender p x == (x :: BSL.ByteString)
                && mimeUnrender p x == Right x

        it "is fromStrict/toStrict (Strict ByteString)" $ do
            property $ \x -> mimeRender p x == BSL.fromStrict (x :: ByteString)
                && mimeUnrender p (BSL.fromStrict x) == Right x

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

        it "returns Just if the 'Accept' header matches, with multiple mime types" $ do
            handleAcceptH (Proxy :: Proxy '[JSONorText]) "application/json" (3 :: Int)
                `shouldSatisfy` isJust
            handleAcceptH (Proxy :: Proxy '[JSONorText]) "text/plain" (3 :: Int)
                `shouldSatisfy` isJust
            handleAcceptH (Proxy :: Proxy '[JSONorText]) "image/jpeg" (3 :: Int)
                `shouldBe` Nothing

        it "returns the Content-Type as the first element of the tuple" $ do
            handleAcceptH (Proxy :: Proxy '[JSON]) "*/*" (3 :: Int)
                `shouldSatisfy` ((== "application/json;charset=utf-8") . fst . fromJust)
            handleAcceptH (Proxy :: Proxy '[PlainText, JSON]) "application/json" (3 :: Int)
                `shouldSatisfy` ((== "application/json;charset=utf-8") . fst . fromJust)
            handleAcceptH (Proxy :: Proxy '[PlainText, JSON, OctetStream])
                "application/octet-stream" ("content" :: ByteString)
                `shouldSatisfy` ((== "application/octet-stream") . fst . fromJust)

        it "returns the appropriately serialized representation" $ do
            property $ \x -> handleAcceptH (Proxy :: Proxy '[JSON]) "*/*" (x :: SomeData)
                == Just ("application/json;charset=utf-8", encode x)

        it "respects the Accept spec ordering" $ do
            let highest a b c = last $ sortBy (compare `on` snd)
-- when qualities are same, http-media-0.8 picks first; 0.7 last.
#if MIN_VERSION_http_media(0,8,0)
                        [ ("text/plain;charset=utf-8", c)
                        , ("application/json;charset=utf-8", b)
                        , ("application/octet-stream", a)
                        ]
#else
                        [ ("application/octet-stream", a)
                        , ("application/json;charset=utf-8", b)
                        , ("text/plain;charset=utf-8", c)
                        ]
#endif
            let acceptH a b c = addToAccept (Proxy :: Proxy OctetStream) a $
                                addToAccept (Proxy :: Proxy JSON)        b $
                                addToAccept (Proxy :: Proxy PlainText )  c $
                                ""
            let val a b c i = handleAcceptH (Proxy :: Proxy '[OctetStream, JSON, PlainText])
                                            (acceptH a b c) (i :: Int)
            property $ \a b c i ->
                let acc = acceptH a b c
                in counterexample (show acc) $
                    fst (fromJust $ val a b c i) === fst (highest a b c)

    describe "handleCTypeH" $ do

        it "returns Nothing if the 'Content-Type' header doesn't match" $ do
            handleCTypeH (Proxy :: Proxy '[JSON]) "text/plain" "𝓽𝓱𝓮 𝓽𝓲𝓶𝓮 𝓱𝓪𝓼 𝓬𝓸𝓶𝓮, 𝓽𝓱𝓮 𝔀𝓪𝓵𝓻𝓾𝓼 𝓼𝓪𝓲𝓭 "
                `shouldBe` (Nothing :: Maybe (Either String Value))

        context "the 'Content-Type' header matches" $ do
            it "returns Just if the parameter matches" $ do
                handleCTypeH (Proxy :: Proxy '[JSON]) "application/json"
                    "𝕥𝕠 𝕥𝕒𝕝𝕜 𝕠𝕗 𝕞𝕒𝕟𝕪 𝕥𝕙𝕚𝕟𝕘𝕤 "
                    `shouldSatisfy` (isJust :: Maybe (Either String Value) -> Bool)

            it "returns Just if there is no parameter" $ do
                handleCTypeH (Proxy :: Proxy '[JSON]) "application/json"
                    "𝕥𝕠 𝕥𝕒𝕝𝕜 𝕠𝕗 𝕞𝕒𝕟𝕪 𝕥𝕙𝕚𝕟𝕘𝕤 "
                    `shouldSatisfy` (isJust :: Maybe (Either String Value) -> Bool)

            it "returns Just Left if the decoding fails" $ do
                let isJustLeft :: Maybe (Either String Value) -> Bool
                    isJustLeft (Just (Left _)) = True
                    isJustLeft _ = False
                handleCTypeH (Proxy :: Proxy '[JSON]) "application/json"
                    "𝕺𝖋 𝖘𝖍𝖔𝖊𝖘--𝖆𝖓𝖉 𝖘𝖍𝖎𝖕𝖘--𝖆𝖓𝖉 𝖘𝖊𝖆𝖑𝖎𝖓𝖌-𝖜𝖆𝖝-- "
                    `shouldSatisfy` isJustLeft

            it "returns Just (Right val) if the decoding succeeds" $ do
                let val = SomeData "Of cabbages--and kings" 12
                handleCTypeH (Proxy :: Proxy '[JSON]) "application/json"
                    (encode val)
                    `shouldBe` Just (Right val)

            it "returns Just (Right val) if the decoding succeeds for either of multiple mime-types" $ do
                let val = 42 :: Int
                handleCTypeH (Proxy :: Proxy '[JSONorText]) "application/json"
                    "42" `shouldBe` Just (Right val)
                handleCTypeH (Proxy :: Proxy '[JSONorText]) "text/plain"
                    "42" `shouldBe` Just (Right val)
                handleCTypeH (Proxy :: Proxy '[JSONorText]) "image/jpeg"
                    "42" `shouldBe` (Nothing :: Maybe (Either String Int))

            it "passes content-type to mimeUnrenderWithType" $ do
                let val = "foobar" :: TextS.Text
                handleCTypeH (Proxy :: Proxy '[JSONorText]) "application/json"
                    "\"foobar\"" `shouldBe` Just (Right val)
                handleCTypeH (Proxy :: Proxy '[JSONorText]) "text/plain"
                    "foobar" `shouldBe` Just (Right val)
                handleCTypeH (Proxy :: Proxy '[JSONorText]) "image/jpeg"
                    "foobar" `shouldBe` (Nothing :: Maybe (Either String Int))

    -- aeson >= 0.9 decodes top-level strings
    describe "eitherDecodeLenient" $ do

        it "parses top-level strings" $ do
            let toMaybe = either (const Nothing) Just
            -- The Left messages differ, so convert to Maybe
            property $ \x -> toMaybe (eitherDecodeLenient x)
                `shouldBe` (decode x :: Maybe String)


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
    mimeRender _ = cs . show

instance MimeRender PlainText Int where
    mimeRender _ = cs . show

instance MimeRender PlainText ByteString where
    mimeRender _ = cs

instance ToJSON ByteString where
    toJSON x = object [ "val" .= x ]

instance IsString AcceptHeader where
    fromString = AcceptHeader . fromString

-- To test multiple content types
data JSONorText

instance Accept JSONorText where
    contentTypes _ = "text/plain" NE.:| [ "application/json" ]

instance MimeRender JSONorText Int  where
    mimeRender _ = cs . show

instance MimeUnrender JSONorText Int where
    mimeUnrender _ = maybe (Left "") Right . readMaybe . BSL8.unpack

instance MimeUnrender JSONorText TextS.Text where
    mimeUnrenderWithType _ mt
        | mt == "application/json" = maybe (Left "") Right . decode
        | otherwise                = Right . TextSE.decodeUtf8 . BSL.toStrict

addToAccept :: Accept a => Proxy a -> ZeroToOne -> AcceptHeader -> AcceptHeader
addToAccept p (ZeroToOne f) (AcceptHeader h) = AcceptHeader (cont h)
    where new = cs (show $ contentType p) `append` "; q=" `append` pack (show f)
          cont "" = new
          cont old = old `append` ", " `append` new
