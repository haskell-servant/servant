module Main (main) where

import qualified Servant.DocsSpec
import Test.Tasty
  ( defaultMain
  )

main :: IO ()
main = defaultMain Servant.DocsSpec.spec
