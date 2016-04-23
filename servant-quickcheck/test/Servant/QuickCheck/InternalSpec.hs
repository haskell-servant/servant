{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.QuickCheck.InternalSpec (spec) where

import Control.Concurrent.MVar (newMVar, readMVar, swapMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Data.Proxy
import Servant
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.IO
import Test.QuickCheck.Monadic

import Servant.QuickCheck.Internal

spec :: Spec
spec = do
  serversEqualSpec

serversEqualSpec :: Spec
serversEqualSpec = describe "serversEqual" $ do

  it "considers equal servers equal" $ do
    withServantServer api server $ \burl1 ->
      withServantServer api server $ \burl2 -> do
        serversEqual api burl1 burl2 stdArgs { maxSuccess = 10000 }



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

noOfTestCases :: Int
#if LONG_TESTS
noOfTestCases = 20000
#else
noOfTestCases = 500
#endif
