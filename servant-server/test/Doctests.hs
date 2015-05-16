module Main where

import           Data.List (isPrefixOf)
import           System.Directory
import           System.FilePath
import           System.FilePath.Find
import           Test.DocTest

main :: IO ()
main = do
    files <- find always (extension ==? ".hs") "src"
    cabalMacrosFile <- getCabalMacrosFile
    doctest $ [ "-isrc"
              , "-optP-include"
              , "-optP" ++ cabalMacrosFile
              , "-XOverloadedStrings"
              , "-XFlexibleInstances"
              , "-XMultiParamTypeClasses"
              , "-XDataKinds"
              , "-XTypeOperators"
              ] ++ files

getCabalMacrosFile :: IO FilePath
getCabalMacrosFile = do
  contents <- getDirectoryContents "dist"
  let rest = "build" </> "autogen" </> "cabal_macros.h"
  return $ case filter ("dist-sandbox-" `isPrefixOf`) contents of
    [x] -> "dist" </> x </> rest
    [] -> "dist" </> rest
    xs -> error $ "ran doctests with multiple dist/dist-sandbox-xxxxx's: \n"
                ++ show xs ++ "\nTry cabal clean"
