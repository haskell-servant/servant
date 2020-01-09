module Main where

import qualified JavascriptSpec

import Test.Hspec (Spec, hspec, describe)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Javascript" JavascriptSpec.spec
