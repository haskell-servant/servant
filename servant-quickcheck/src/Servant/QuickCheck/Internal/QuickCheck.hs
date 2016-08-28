{-# LANGUAGE RecordWildCards #-}
module Servant.QuickCheck.Internal.QuickCheck where

import qualified Data.ByteString.Lazy     as LBS
import           Data.Proxy               (Proxy)
import           Data.String              (IsString (..))
import           Data.Text                (Text)
import           GHC.Generics             (Generic)
import qualified Network.HTTP.Client      as C
import           Network.Wai.Handler.Warp (withApplication)
import           Prelude.Compat
import           Servant                  (Context (EmptyContext), HasServer,
                                           Server, serveWithContext)
import           Servant.Client           (BaseUrl (..), Scheme (..))
import           System.IO.Unsafe         (unsafePerformIO)
import           Test.Hspec               (Expectation, expectationFailure)
import           Test.QuickCheck          (Args (..), Result (..),
                                           quickCheckWithResult)
import           Test.QuickCheck.Monadic  (assert, forAllM, monadicIO, run, monitor)
import           Test.QuickCheck.Property (counterexample)
import Control.Monad (unless)
import Control.Concurrent (newMVar, modifyMVar_, readMVar)

import Servant.QuickCheck.Internal.Equality
import Servant.QuickCheck.Internal.HasGenRequest
import Servant.QuickCheck.Internal.Predicates
import Servant.QuickCheck.Internal.ErrorTypes


-- | Start a servant application on an open port, run the provided function,
-- then stop the application.
--
-- /Since 0.0.0.0/
withServantServer :: HasServer a '[] => Proxy a -> IO (Server a)
  -> (BaseUrl -> IO r) -> IO r
withServantServer api = withServantServerAndContext api EmptyContext

-- | Like 'withServantServer', but allows passing in a 'Context' to the
-- application.
--
-- /Since 0.0.0.0/
withServantServerAndContext :: HasServer a ctx
  => Proxy a -> Context ctx -> IO (Server a) -> (BaseUrl -> IO r) -> IO r
withServantServerAndContext api ctx server t
  = withApplication (return . serveWithContext api ctx =<< server) $ \port ->
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
--
-- /Since 0.0.0.0/
serversEqual :: HasGenRequest a =>
  Proxy a -> BaseUrl -> BaseUrl -> Args -> ResponseEquality LBS.ByteString -> Expectation
serversEqual api burl1 burl2 args req = do
  let reqs = (\f -> (f burl1, f burl2)) <$> genRequest api
  -- This MVar stuff is clunky! But there doesn't seem to be an easy way to
  -- return results when a test fails, since an exception is throw.
  deetsMVar <- newMVar $ error "should not be called"
  r <- quickCheckWithResult args { chatty = False } $ monadicIO $ forAllM reqs $ \(req1, req2) -> do
    resp1 <- run $ C.httpLbs (noCheckStatus req1) defManager
    resp2 <- run $ C.httpLbs (noCheckStatus req2) defManager
    unless (getResponseEquality req resp1 resp2) $ do
      monitor (counterexample "hi" )
      run $ modifyMVar_ deetsMVar $ const $ return $
        ServerEqualityFailure req1 resp1 resp2
      assert False
  case r of
    Success {} -> return ()
    f@Failure{..} -> readMVar deetsMVar >>= \x -> expectationFailure $
      "Failed:\n" ++ show x
    GaveUp { numTests = n } -> expectationFailure $ "Gave up after " ++ show n ++ " tests"
    NoExpectedFailure {} -> expectationFailure $ "No expected failure"
    InsufficientCoverage {} -> expectationFailure $ "Insufficient coverage"

-- | Check that a server satisfies the set of properties specified.
--
-- Note that, rather than having separate tests for each property you'd like to
-- test, you should generally prefer to combine all properties into a single
-- test. This enables a more parsimonious generation of requests and responses
-- with the same testing depth.
--
-- Example usage:
--
-- > goodAPISpec = describe "my server" $ do
-- >
-- >   it "follows best practices" $ do
-- >     withServantServer api server $ \burl ->
-- >       serverSatisfies api burl stdArgs (not500
-- >                                     <%> onlyJsonObjects
-- >                                     <%> notAllowedContainsAllowHeader
-- >                                     <%> mempty)
--
-- /Since 0.0.0.0/
serverSatisfies :: (HasGenRequest a) =>
  Proxy a -> BaseUrl -> Args -> Predicates -> Expectation
serverSatisfies api burl args preds = do
  let reqs = ($ burl) <$> genRequest api
  deetsMVar <- newMVar $ error "should not be called"
  r <- quickCheckWithResult args { chatty = False } $ monadicIO $ forAllM reqs $ \req -> do
     v <- run $ finishPredicates preds (noCheckStatus req) defManager
     run $ modifyMVar_ deetsMVar $ const $ return v
     case v of
       Just x -> assert False
       _ -> return ()
  case r of
    Success {} -> return ()
    f@Failure{..} -> readMVar deetsMVar >>= \x -> expectationFailure $
      "Failed:\n" ++ show x
    GaveUp { numTests = n } -> expectationFailure $ "Gave up after " ++ show n ++ " tests"
    NoExpectedFailure {} -> expectationFailure $ "No expected failure"
    InsufficientCoverage {} -> expectationFailure $ "Insufficient coverage"


serverDoesntSatisfy :: (HasGenRequest a) =>
  Proxy a -> BaseUrl -> Args -> Predicates -> Expectation
serverDoesntSatisfy api burl args preds = do
  let reqs = ($ burl) <$> genRequest api
  r <- quickCheckWithResult args $ monadicIO $ forAllM reqs $ \req -> do
     v <- run $ finishPredicates preds (noCheckStatus req) defManager
     assert $ not $ null v
  case r of
    Success {} -> return ()
    GaveUp { numTests = n } -> expectationFailure $ "Gave up after " ++ show n ++ " tests"
    Failure { output = m } -> expectationFailure $ "Failed:\n" ++ show m
    NoExpectedFailure {} -> expectationFailure $ "No expected failure"
    InsufficientCoverage {} -> expectationFailure $ "Insufficient coverage"

noCheckStatus :: C.Request -> C.Request
noCheckStatus r = r { C.checkStatus = \_ _ _ -> Nothing}

defManager :: C.Manager
defManager = unsafePerformIO $ C.newManager C.defaultManagerSettings
{-# NOINLINE defManager #-}
