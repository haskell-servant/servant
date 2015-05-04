module Main where

import           System.FilePath.Find
import           Test.DocTest

main :: IO ()
main = do
    files <- find always (extension ==? ".hs") "src"
    doctest $ [ "-isrc"
              , "-optP-include"
              , "-optPdist/build/autogen/cabal_macros.h"
              , "-XOverloadedStrings"
              , "-XFlexibleInstances"
              , "-XMultiParamTypeClasses"
              , "-XDataKinds"
              , "-XTypeOperators"
              ] ++ files

