module Main where

import           Data.List (isPrefixOf)
import           System.Directory
import           System.FilePath
import           System.FilePath.Find
import           Test.DocTest

main :: IO ()
main = do
    files <- find always (extension ==? ".hs") "src"
    doctest $ [ "-isrc"
              , "-XOverloadedStrings"
              , "-XFlexibleInstances"
              , "-XMultiParamTypeClasses"
              ] ++ files
