module Main where

import           Data.List            (isPrefixOf)
import           System.Directory
import           System.FilePath
import           System.FilePath.Find
import           Test.DocTest

main :: IO ()
main = do
    files <- find always (extension ==? ".hs") "src"
    mCabalMacrosFile <- getCabalMacrosFile
    doctest $ "-isrc" :
              (maybe [] (\ f -> ["-optP-include", "-optP" ++ f]) mCabalMacrosFile) ++
              "-XOverloadedStrings" :
              "-XFlexibleInstances" :
              "-XMultiParamTypeClasses" :
              "-XDataKinds" :
              "-XTypeOperators" :
              files

getCabalMacrosFile :: IO (Maybe FilePath)
getCabalMacrosFile = do
  exists <- doesDirectoryExist "dist"
  if exists
    then do
      contents <- getDirectoryContents "dist"
      let rest = "build" </> "autogen" </> "cabal_macros.h"
      whenExists $ case filter ("dist-sandbox-" `isPrefixOf`) contents of
        [x] -> "dist" </> x </> rest
        [] -> "dist" </> rest
        xs -> error $ "ran doctests with multiple dist/dist-sandbox-xxxxx's: \n"
                    ++ show xs ++ "\nTry cabal clean"
    else return Nothing
 where
  whenExists :: FilePath -> IO (Maybe FilePath)
  whenExists file = do
    exists <- doesFileExist file
    return $ if exists
      then Just file
      else Nothing
