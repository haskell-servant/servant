{-# LANGUAGE OverloadedStrings, DataKinds #-}
module Main (main) where

import Servant
import Servant.QuickCheck
import Test.Hspec
import Data.Text (Text)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Not running without arguments. Try --help or --fail-fast."
        _  -> hspec spec

-- Change to String to reproduce
-- https://github.com/haskell-servant/servant-quickcheck/issues/41
type API = Get '[PlainText] Text

api :: Proxy API
api = Proxy

server :: Server API
server = return "Sigurð Fáfnirslayer"

spec :: Spec
spec = describe "example server" $
    it "mangles UTF-8 in error messages" $
        withServantServer api (return server) $ \burl ->
            serverSatisfies api burl defaultArgs (getsHaveCacheControlHeader <%> mempty)
