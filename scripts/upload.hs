#!/usr/bin/env stack
{- stack
--resolver lts-3.10
--install-ghc runghc
-}

import           Data.Foldable
import           System.Process

main :: IO ()
main = do
  sources <- words <$> readFile "sources.txt"
  forM_ sources $ \ source -> do
    callCommand ("stack upload " ++ source)
