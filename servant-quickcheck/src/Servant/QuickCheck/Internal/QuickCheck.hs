-- | This module contains wrappers around lower-level functionality.
module Servant.QuickCheck.Internal.QuickCheck where

import           Data.Proxy               (Proxy)
import           Network.HTTP.Client      (Manager, defaultManagerSettings,
                                           newManager, httpLbs, checkStatus, Request)
import           Network.Wai.Handler.Warp (withApplication)
import           Servant                  (HasServer, Server, serve)
import           Servant.Client           (BaseUrl (..), Scheme (..) )
import           Test.Hspec               (Expectation, expectationFailure)
import           Test.QuickCheck          (Args (..), Result (..),
                                           quickCheckWithResult)
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck.Monadic
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)

import Servant.QuickCheck.Internal.HasGenRequest
import Servant.QuickCheck.Internal.Predicates
import Servant.QuickCheck.Internal.Equality


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
  Proxy a -> BaseUrl -> BaseUrl -> Args -> ResponseEquality LBS.ByteString -> Expectation
serversEqual api burl1 burl2 args req = do
  let reqs = (\f -> (f burl1, f burl2)) <$> genRequest api
  r <- quickCheckWithResult args $ monadicIO $ forAllM reqs $ \(req1, req2) -> do
    resp1 <- run $ httpLbs (noCheckStatus req1) defManager
    resp2 <- run $ httpLbs (noCheckStatus req2) defManager
    assert $ getResponseEquality req resp1 resp2
  case r of
    Success {} -> return ()
    GaveUp { numTests = n } -> expectationFailure $ "Gave up after " ++ show n ++ " tests"
    Failure { output = m } -> expectationFailure $ "Failed:\n" ++ show m
    NoExpectedFailure {} -> expectationFailure $ "No expected failure"
    InsufficientCoverage {} -> expectationFailure $ "Insufficient coverage"

serverSatisfies :: (HasGenRequest a) =>
  Proxy a -> BaseUrl -> Args -> Predicates [Text] [Text] -> Expectation
serverSatisfies api burl args preds = do
  let reqs = ($ burl) <$> genRequest api
  r <- quickCheckWithResult args $ monadicIO $ forAllM reqs $ \req -> do
     v <- run $ finishPredicates preds (noCheckStatus req) defManager
     {-run $ print v-}
     assert $ null v
  case r of
    Success {} -> return ()
    GaveUp { numTests = n } -> expectationFailure $ "Gave up after " ++ show n ++ " tests"
    Failure { output = m } -> expectationFailure $ "Failed:\n" ++ show m
    NoExpectedFailure {} -> expectationFailure $ "No expected failure"
    InsufficientCoverage {} -> expectationFailure $ "Insufficient coverage"


noCheckStatus :: Request -> Request
noCheckStatus r = r { checkStatus = \_ _ _ -> Nothing}

defManager :: Manager
defManager = unsafePerformIO $ newManager defaultManagerSettings
{-# NOINLINE defManager #-}
