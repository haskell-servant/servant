{-# LANGUAGE CPP #-}
#ifdef __GHCJS__
module Main (main) where
main :: IO ()
main = return ()
#else
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
#endif
