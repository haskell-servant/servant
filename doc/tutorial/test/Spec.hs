module Main where

import qualified JavascriptSpec

import Test.Hspec (Spec, describe, hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Javascript" JavascriptSpec.spec
