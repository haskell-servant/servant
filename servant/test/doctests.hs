-----------------------------------------------------------------------------
-- |
-- Module      :  Main (doctests)
-- Copyright   :  (C) 2012-14 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides doctests for a project based on the actual versions
-- of the packages it was built with. It requires a corresponding Setup.lhs
-- to be added to the project
-----------------------------------------------------------------------------
module Main where

import Build_doctests (flags, pkgs, module_sources)
import Data.Foldable (traverse_)
import Data.List (isPrefixOf, isSuffixOf)
import Test.DocTest

main :: IO ()
main = do
    traverse_ putStrLn args
    doctest args
  where
    args = 
      "-XOverloadedStrings" :
      "-XFlexibleInstances" :
      "-XMultiParamTypeClasses" :
      flags ++ flags' ++ pkgs ++
      "Servant.Utils.LinksSpec" :
      module_sources

    -- HACK: find -i.../src, and change it ot -i.../test
    flags'
        = map (\x -> take (length x - 4) x ++ "/test")
        . filter (\x -> "-i" `isPrefixOf` x && "/src" `isSuffixOf` x)
        $ flags

