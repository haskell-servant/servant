{-# LANGUAGE OverloadedStrings #-}
module Servant.API.StreamSpec where

import           Control.Monad.Except
                 (runExcept)
import qualified Data.Aeson                as Aeson
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LBS
import           Data.Functor.Identity
                 (Identity (..))
import           Data.Proxy
                 (Proxy (..))
import           Data.String
                 (fromString)
import           Servant.API.Stream
import           Servant.Types.SourceT
import           Test.Hspec
import           Test.QuickCheck
                 (Property, property, (===))
import           Test.QuickCheck.Instances ()

spec :: Spec
spec = describe "Servant.API.Stream" $ do
    describe "NoFraming" $ do
        let framingUnrender' = framingUnrender (Proxy :: Proxy NoFraming) (Right . LBS.toStrict)
            framingRender' = framingRender (Proxy :: Proxy NoFraming) LBS.fromStrict

        it "framingUnrender" $
            property $ \bss ->
                runUnrenderFrames framingUnrender' bss === map Right (bss :: [BS.ByteString])

        it "roundtrip" $
            property $ roundtrip framingRender' framingUnrender'

    describe "NewlineFraming" $ do
        let tp = framingUnrender (Proxy :: Proxy NewlineFraming) (Right . LBS.toStrict)
        let re = framingRender (Proxy :: Proxy NewlineFraming) id

        it "framingRender examples" $ do
            runRenderFrames re [] `shouldBe` Right ""
            runRenderFrames re ["foo", "bar", "baz"] `shouldBe` Right "foo\nbar\nbaz\n"

        it "framingUnrender examples" $ do
            let expected n = map Right [fromString ("foo" ++ show (n :: Int)), "bar", "baz"]

            runUnrenderFrames tp ["foo1\nbar\nbaz"]         `shouldBe` expected 1
            runUnrenderFrames tp ["foo2\n", "bar\n", "baz"] `shouldBe` expected 2
            runUnrenderFrames tp ["foo3\nb", "ar\nbaz"]     `shouldBe` expected 3

        it "roundtrip" $ do
            let framingUnrender' = framingUnrender (Proxy :: Proxy NewlineFraming) Aeson.eitherDecode
            let framingRender'  = framingRender (Proxy :: Proxy NewlineFraming) (Aeson.encode :: Int -> LBS.ByteString)

            property $ roundtrip framingRender' framingUnrender'

        -- it "fails if input doesn't contain newlines often" $
        --    runUnrenderFrames tp ["foo", "bar"] `shouldSatisfy` any isLeft

    describe "NetstringFraming" $ do
        let tp = framingUnrender (Proxy :: Proxy NetstringFraming) (Right . LBS.toStrict)
        let re = framingRender (Proxy :: Proxy NetstringFraming) id

        it "framingRender examples" $ do
            runRenderFrames re [] `shouldBe` Right ""
            runRenderFrames re ["foo", "bar", "baz"] `shouldBe` Right "3:foo,3:bar,3:baz,"

        it "framingUnrender examples" $ do
            let expected n = map Right [fromString ("foo" ++ show (n :: Int)), "bar", "baz"]

            runUnrenderFrames tp ["4:foo1,3:bar,3:baz,"]         `shouldBe` expected 1
            runUnrenderFrames tp ["4:foo2,", "3:bar,", "3:baz,"] `shouldBe` expected 2
            runUnrenderFrames tp ["4:foo3,3:b", "ar,3:baz,"]     `shouldBe` expected 3

        it "roundtrip" $ do
            let framingUnrender' = framingUnrender (Proxy :: Proxy NetstringFraming) Aeson.eitherDecode
            let framingRender'  = framingRender (Proxy :: Proxy NetstringFraming) (Aeson.encode :: Int -> LBS.ByteString)

            property $ roundtrip framingRender' framingUnrender'

roundtrip
    :: (Eq a, Show a)
    => (SourceT Identity a -> SourceT Identity LBS.ByteString)
    -> (SourceT Identity BS.ByteString -> SourceT Identity a)
    -> [a]
    -> Property
roundtrip render unrender xs =
    map Right xs === runUnrenderFrames (unrender . fmap LBS.toStrict . render) xs

runRenderFrames :: (SourceT Identity a -> SourceT Identity LBS.ByteString) -> [a] -> Either String LBS.ByteString
runRenderFrames f = fmap mconcat . runExcept . runSourceT . f . source

runUnrenderFrames :: (SourceT Identity b -> SourceT Identity a) -> [b] -> [Either String a]
runUnrenderFrames f = go . Effect . (\s -> unSourceT s return) . f . source where
    go :: StepT Identity a -> [Either String a]
    go Stop        = []
    go (Error err) = [Left err]
    go (Skip s)    = go s
    go (Yield x s) = Right x :  go s
    go (Effect ms) = go (runIdentity ms)
