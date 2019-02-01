{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

#ifndef MIN_VERSION_cabal_doctest
#define MIN_VERSION_cabal_doctest(x,y,z) 0
#endif

#if MIN_VERSION_cabal_doctest(1,0,0)

import Distribution.Extra.Doctest ( defaultMainWithDoctests )
main :: IO ()
main = defaultMainWithDoctests "doctests"

#else

#ifdef MIN_VERSION_Cabal
-- If the macro is defined, we have new cabal-install,
-- but for some reason we don't have cabal-doctest in package-db
--
-- Probably we are running cabal sdist, when otherwise using new-build
-- workflow
#warning You are configuring this package without cabal-doctest installed. \
         The doctests test-suite will not work as a result. \
         To fix this, install cabal-doctest before configuring.
#endif

import Distribution.Simple

main :: IO ()
main = defaultMain

#endif
