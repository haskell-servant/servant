module Servant.Common.TextSpec where

import           Data.Int                  (Int16, Int32, Int64, Int8)
import           Data.Text                 (Text)
import           Data.Word                 (Word, Word16, Word32, Word64, Word8)
import           Servant.Common.Text
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

spec :: Spec
spec = describe "Servant.Common.Text" $ do

    context "FromText and ToText laws" $ do

        it "holds for Text" $
            property $ \x -> textLaw (x :: Text)

        it "holds for String" $
            property $ \x -> textLaw (x :: String)

        it "holds for Bool" $
            property $ \x -> textLaw (x :: Bool)

        it "holds for Int" $
            property $ \x -> textLaw (x :: Int)

        it "holds for Int8" $
            property $ \x -> textLaw (x :: Int8)

        it "holds for Int16" $
            property $ \x -> textLaw (x :: Int16)

        it "holds for Int32" $
            property $ \x -> textLaw (x :: Int32)

        it "holds for Int64" $
            property $ \x -> textLaw (x :: Int64)

        it "holds for Word" $
            property $ \x -> textLaw (x :: Word)

        it "holds for Word8" $
            property $ \x -> textLaw (x :: Word8)

        it "holds for Word16" $
            property $ \x -> textLaw (x :: Word16)

        it "holds for Word32" $
            property $ \x -> textLaw (x :: Word32)

        it "holds for Word64" $
            property $ \x -> textLaw (x :: Word64)

        it "holds for Integer" $
            property $ \x -> textLaw (x :: Integer)

        -- The following two properties are only reasonably expected to hold up
        -- to a certain precision.
        --
        -- http://en.wikipedia.org/wiki/Floating_point#Internal_representation
        it "holds for Double" $
            property $ \x ->
                x < 1.0e15 && x > 1.0e-15 ==>
                    textLaw (x :: Double)

        it "holds for Float" $
            property $ \x ->
                x < 1.0e7 && x > 1.0e-7 ==>
                    textLaw (x :: Float)

textLaw :: (FromText a, ToText a, Eq a) => a -> Bool
textLaw a = fromText (toText a) == Just a
