-- | This module contains wrappers around lower-level functionality.
module Servant.QuickCheck.Internal.QuickCheck where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.MVar  (modifyMVar_, readMVar)
import           Control.Monad            (replicateM_)
import           Data.Proxy               (Proxy)
import           Data.Void                (Void)
import           Network.HTTP.Client      (Manager, defaultManagerSettings,
                                           newManager, httpLbs)
import           Network.HTTP.Client      (managerModifyRequest, getUri)
import           Network.Wai.Handler.Warp (withApplication)
import           Servant                  (HasServer, Server, serve)
import           Servant.Client           (BaseUrl (..), Client, HasClient,
                                           Scheme (..), ServantError, client)
import           System.IO                (hPutStrLn, hFlush)
import           System.IO.Temp           (withSystemTempFile)
import           System.Mem               (performGC)
import           System.Process           (callCommand)
import           Test.Hspec               (Expectation, expectationFailure)
import           Test.QuickCheck          (Args (..), Property, forAll, Result (..),
                                           Testable, property, ioProperty,
                                           quickCheckWithResult, stdArgs)

import Servant.QuickCheck.Internal.HasGenRequest
import Servant.QuickCheck.Internal.Predicates
import Servant.QuickCheck.Internal.Benchmarking


-- | Start a servant application on an open port, run the provided function,
-- then stop the application.
withServantServer :: HasServer a '[] => Proxy a -> IO (Server a)
  -> (BaseUrl -> IO r) -> IO r
withServantServer api server t
  = withApplication (return . serve api =<< server) $ \port ->
      t (BaseUrl Http "localhost" port "")

-- | Check that the two servers running under the provided @BaseUrl@s behave
-- identically by randomly generating arguments (captures, query params, request bodies,
-- headers, etc.) expected by the server. If, given the same request, the
-- response is not the same (according to the definition of @==@ for the return
-- datatype), the 'Expectation' fails, printing the counterexample.
--
-- The @Int@ argument specifies maximum number of test cases to generate and
-- run.
--
-- Evidently, if the behaviour of the server is expected to be
-- non-deterministic,  this function may produce spurious failures
serversEqual :: HasGenRequest a =>
  Proxy a -> BaseUrl -> BaseUrl -> Manager -> Property
serversEqual api burl1 burl2 mgr =
  let reqs = (\f -> (f burl1, f burl2)) <$> genRequest api
  in forAll reqs $ \(req1, req2) -> ioProperty $ do
    resp1 <- httpLbs req1 mgr
    resp2 <- httpLbs req2 mgr
    return $ resp1 == resp2


