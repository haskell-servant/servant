{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE PolyKinds             #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Servant.API.ContentTypesSpec where

import           Prelude ()
import           Prelude.Compat

import           Control.Arrow
import           Control.Monad             (when)
import           Control.Monad.Except      (runExceptT)
import           Control.Monad.Trans       (liftIO)
import           Data.Aeson
import           Data.ByteString.Char8     (ByteString, append, pack)
import qualified Data.ByteString.Lazy      as BSL
import           Data.Either
import           Data.Function             (on)
import           Data.List                 (maximumBy)
import           Data.Maybe                (fromJust, isJust, isNothing)
import           Data.Proxy
import           Data.String               (IsString (..))
import           Data.String.Conversions   (cs)
import qualified Data.Text                 as TextS
import qualified Data.Text.Lazy            as TextL
import           GHC.Generics
import           Network.URL               (exportParams, importParams)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import "quickcheck-instances" Test.QuickCheck.Instances ()

import           Servant.API.ContentTypes

shouldUnrenderTo :: (MimeUnrender ct a, Eq a, Show a) => (Proxy ct, BSL.ByteString) -> a -> IO ()
shouldUnrenderTo (p, bs) x = do
  res <- runExceptT (mimeUnrender p bs)
  res `shouldBe` Right x

shouldNotUnrenderTo :: forall ct a. (MimeUnrender ct a, Show a) =>
  (Proxy ct, BSL.ByteString) -> Proxy a -> IO ()
shouldNotUnrenderTo (p, bs) _ = do
  res <- runExceptT (mimeUnrender p bs)
  res `shouldSatisfy` (isLeft :: Either e a -> Bool)

shouldHandleCTypeH :: (AllCTUnrender cts a, Eq a, Show a) =>
  (Proxy cts, BSL.ByteString, BSL.ByteString) -> a -> IO ()
shouldHandleCTypeH (p, ct, bs) x = do
  res <- traverse runExceptT (handleCTypeH p ct bs)
  res `shouldBe` Just (Right x)

shouldNotFindCTypeH :: forall cts a. (AllCTUnrender cts a, Eq a, Show a) =>
  (Proxy cts, BSL.ByteString, BSL.ByteString) -> Proxy a -> IO ()
shouldNotFindCTypeH (p, ct, bs) _ = do
  res <- traverse runExceptT (handleCTypeH p ct bs)
  res `shouldBe` (Nothing :: Maybe (Either String a))

shouldHandleCTypeHSatisfy :: forall cts a. (AllCTUnrender cts a, Show a) =>
  (Proxy cts, BSL.ByteString, BSL.ByteString) -> (Maybe (Either String a) -> Bool) -> IO ()
shouldHandleCTypeHSatisfy (p, ct, bs) f = do
  res <- traverse runExceptT (handleCTypeH p ct bs)
  res `shouldSatisfy` f

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
            (p, "[1] ") `shouldUnrenderTo` [1 :: Int]

        it "handles whitespace at beginning of input" $ do
            (p, " [1]") `shouldUnrenderTo` [1 :: Int]

        it "does not like junk at end of input" $ do
            (p, "[1] this probably shouldn't work")
              `shouldNotUnrenderTo` (Proxy :: Proxy [Int])

        it "has mimeUnrender reverse mimeRender for valid top-level json ([Int]) " $ do
            property $ \x -> monadicIO $ do
              res <- liftIO $ runExceptT (mimeUnrender p (mimeRender p x))
              assert (res == Right (x::[Int]))

        it "has mimeUnrender reverse mimeRender for valid top-level json " $ do
            property $ \x -> monadicIO $ do
              res <- liftIO $ runExceptT (mimeUnrender p (mimeRender p x))
              assert (res == Right (x::[SomeData]))

    describe "The FormUrlEncoded Content-Type type" $ do
        let p = Proxy :: Proxy FormUrlEncoded

        it "has mimeUnrender reverse mimeRender" $ do
            property $ \x -> monadicIO $ when (mempty `notElem` x) $ do
              res <- liftIO . runExceptT $ mimeUnrender p (mimeRender p x)
              assert (res == Right (x::[(TextS.Text,TextS.Text)]))

        it "has mimeUnrender reverse exportParams (Network.URL)" $ do
            property $ \x -> monadicIO $ when (mempty `notElem` x) $ do
              res <- liftIO . runExceptT $ mimeUnrender p . cs . exportParams . map (cs *** cs) $ x
              assert (res == Right (x::[(TextS.Text,TextS.Text)]))

        it "has importParams (Network.URL) reverse mimeRender" $ do
            property $ \x -> mempty `notElem` x
                ==> (fmap (map (cs *** cs)) . importParams . cs . mimeRender p $ x) == Just (x::[(TextS.Text,TextS.Text)])

    describe "The PlainText Content-Type type" $ do
        let p = Proxy :: Proxy PlainText

        it "has mimeUnrender reverse mimeRender (lazy Text)" $ do
            property $ \x -> monadicIO $ do
              res <- liftIO . runExceptT $ mimeUnrender p (mimeRender p x)
              assert (res == Right (x::TextL.Text))

        it "has mimeUnrender reverse mimeRender (strict Text)" $ do
            property $ \x -> monadicIO $ do
              res <- liftIO . runExceptT $ mimeUnrender p (mimeRender p x)
              assert (res == Right (x::TextS.Text))

    describe "The OctetStream Content-Type type" $ do
        let p = Proxy :: Proxy OctetStream

        it "is id (Lazy ByteString)" $ do
            property $ \x -> monadicIO $ do
              assert (mimeRender p x == (x :: BSL.ByteString))
              res <- liftIO . runExceptT $ mimeUnrender p x
              assert (res == Right x)

        it "is fromStrict/toStrict (Strict ByteString)" $ do
            property $ \x -> monadicIO $ do
              assert (mimeRender p x == BSL.fromStrict (x :: ByteString))
              res <- liftIO . runExceptT $ mimeUnrender p (BSL.fromStrict x)
              assert (res == Right x)

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
                == Just ("application/json", encode x)

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
            (Proxy :: Proxy '[JSON], "text/plain", "𝓽𝓱𝓮 𝓽𝓲𝓶𝓮 𝓱𝓪𝓼 𝓬𝓸𝓶𝓮, 𝓽𝓱𝓮 𝔀𝓪𝓵𝓻𝓾𝓼 𝓼𝓪𝓲𝓭 ")
              `shouldNotFindCTypeH` (Proxy :: Proxy Value)

        context "the 'Content-Type' header matches" $ do
            it "returns Just if the parameter matches" $ do
                (Proxy :: Proxy '[JSON], "application/json", "𝕥𝕠 𝕥𝕒𝕝𝕜 𝕠𝕗 𝕞𝕒𝕟𝕪 𝕥𝕙𝕚𝕟𝕘𝕤 ")
                  `shouldHandleCTypeHSatisfy` (isJust :: Maybe (Either String Value) -> Bool)

            it "returns Just if there is no parameter" $ do
                (Proxy :: Proxy '[JSON], "application/json", "𝕥𝕠 𝕥𝕒𝕝𝕜 𝕠𝕗 𝕞𝕒𝕟𝕪 𝕥𝕙𝕚𝕟𝕘𝕤 ")
                  `shouldHandleCTypeHSatisfy` (isJust :: Maybe (Either String Value) -> Bool)

            it "returns Just Left if the decoding fails" $ do
                let isJustLeft :: Maybe (Either String Value) -> Bool
                    isJustLeft (Just (Left _)) = True
                    isJustLeft _ = False
                (Proxy :: Proxy '[JSON], "application/json", "𝕺𝖋 𝖘𝖍𝖔𝖊𝖘--𝖆𝖓𝖉 𝖘𝖍𝖎𝖕𝖘--𝖆𝖓𝖉 𝖘𝖊𝖆𝖑𝖎𝖓𝖌-𝖜𝖆𝖝-- ")
                  `shouldHandleCTypeHSatisfy` isJustLeft

            it "returns Just (Right val) if the decoding succeeds" $ do
                let val = SomeData "Of cabbages--and kings" 12
                (Proxy :: Proxy '[JSON], "application/json", encode val)
                  `shouldHandleCTypeH` val

#if MIN_VERSION_aeson(0,9,0)
    -- aeson >= 0.9 decodes top-level strings
    describe "eitherDecodeLenient" $ do

        it "parses top-level strings" $ do
            let toMaybe = either (const Nothing) Just
            -- The Left messages differ, so convert to Maybe
            property $ \x -> toMaybe (eitherDecodeLenient x)
                `shouldBe` (decode x :: Maybe String)
#endif


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

addToAccept :: Accept a => Proxy a -> ZeroToOne -> AcceptHeader -> AcceptHeader
addToAccept p (ZeroToOne f) (AcceptHeader h) = AcceptHeader (cont h)
    where new = cs (show $ contentType p) `append` "; q=" `append` pack (show f)
          cont "" = new
          cont old = old `append` ", " `append` new
