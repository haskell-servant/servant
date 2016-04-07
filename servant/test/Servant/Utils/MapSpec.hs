module Servant.Utils.MapSpec where

import           Test.Hspec

import           Data.Functor.Identity
import           Servant.API
import           Servant.Utils.Map
spec :: Spec
spec = describe "Map" $ do
  it "maps " $ do
    let foo :: Bool -> Int -> String
        foo _ b = show b

        bar :: () -> String -> Int -> Double
        bar _ _ i = fromIntegral i

        foobar = foo :<|> bar

        convert :: (Int -> a) -> Identity a
        convert f = Identity $ f 42

        foo' :: Bool -> Identity String
        bar' :: () -> String -> Identity Double
        foo' :<|> bar' = mapLeaves convert foobar

    foo' True `shouldBe` Identity "42"
    bar' () "" `shouldBe` Identity (42 :: Double)
