module Main (main) where

import           Test.Tasty
                 (defaultMain)
import qualified Servant.DocsSpec

main :: IO ()
main = defaultMain Servant.DocsSpec.spec
