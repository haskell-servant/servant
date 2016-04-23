{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.QuickCheck.InternalSpec (spec) where

import Control.Concurrent.MVar (newMVar, readMVar, swapMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy
import Servant
import Test.Hspec
import Test.QuickCheck

import Servant.QuickCheck

spec :: Spec
spec = do
  serversEqualSpec
  serverSatisfiesSpec

serversEqualSpec :: Spec
serversEqualSpec = describe "serversEqual" $ do

  it "considers equal servers equal" $ do
    withServantServer api server $ \burl1 ->
      withServantServer api server $ \burl2 -> do
        serversEqual api burl1 burl2 args bodyEquality


serverSatisfiesSpec :: Spec
serverSatisfiesSpec = describe "serverSatisfies" $ do

  it "succeeds for true predicates" $ do
    withServantServer api server $ \burl ->
      serverSatisfies api burl args (not500 <%> mempty)


------------------------------------------------------------------------------
-- APIs
------------------------------------------------------------------------------

type API = ReqBody '[JSON] String :> Post '[JSON] String
      :<|> Get '[JSON] Int

api :: Proxy API
api = Proxy

server :: IO (Server API)
server = do
    mvar <- newMVar ""
    return $ (\x -> liftIO $ swapMVar mvar x)
        :<|> (liftIO $ readMVar mvar >>= return . length)


------------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------------

args :: Args
args = stdArgs { maxSuccess = noOfTestCases }

noOfTestCases :: Int
#if LONG_TESTS
noOfTestCases = 20000
#else
noOfTestCases = 500
#endif
