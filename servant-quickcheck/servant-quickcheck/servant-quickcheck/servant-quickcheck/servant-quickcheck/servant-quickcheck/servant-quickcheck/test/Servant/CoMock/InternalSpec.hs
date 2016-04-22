{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.CoMock.InternalSpec (spec) where

import Control.Concurrent.MVar (newMVar, readMVar, swapMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy
import Servant
import Test.Hspec

import Servant.CoMock.Internal

spec :: Spec
spec = do
  serversEqualSpec
  serverSatisfiesSpec
  serverBenchmarkSpec


serversEqualSpec :: Spec
serversEqualSpec = describe "serversEqual" $ do

  context "servers without function types" $ do

    it "considers equal servers equal" $ do
      withServantServer onlyReturnAPI onlyReturnAPIServer $ \burl ->
        serversEqual onlyReturnAPI burl burl noOfTestCases

    it "considers unequal servers unequal" $ do
      withServantServer onlyReturnAPI onlyReturnAPIServer $ \burl1 ->
        withServantServer onlyReturnAPI onlyReturnAPIServer' $ \burl2 ->
          serversUnequal onlyReturnAPI burl1 burl2 noOfTestCases


  context "servers with function types" $ do

    it "considers equal servers equal" $ do
      withServantServer functionAPI functionAPIServer $ \burl ->
        serversEqual functionAPI burl burl noOfTestCases

    it "considers unequal servers unequal" $ do
      withServantServer functionAPI functionAPIServer $ \burl1 ->
        withServantServer functionAPI functionAPIServer' $ \burl2 ->
          serversUnequal functionAPI burl1 burl2 noOfTestCases


  context "stateful servers" $ do

    it "considers equal servers equal" $ do
      withServantServer statefulAPI statefulAPIServer $ \burl1 ->
        withServantServer statefulAPI statefulAPIServer $ \burl2 ->
           serversEqual statefulAPI burl1 burl2 noOfTestCases


serverSatisfiesSpec :: Spec
serverSatisfiesSpec = describe "serverSatisfies" $ do

  it "passes true predicates" $ do
    let e = addRightPredicate (== (5 :: Int)) emptyPredicates
    withServantServer onlyReturnAPI onlyReturnAPIServer $ \burl ->
      serverSatisfies onlyReturnAPI burl emptyPredicates e noOfTestCases

  it "fails false predicates" $ do
    let e = addRightPredicate (== (4 :: Int)) emptyPredicates
    withServantServer onlyReturnAPI onlyReturnAPIServer $ \burl ->
      serverDoesntSatisfy onlyReturnAPI burl emptyPredicates e noOfTestCases

  it "allows filtering" $ do
    let f  = addPredicate (\(x :: String) -> length x > 2) emptyPredicates
        e  = addRightPredicate (\(x :: Int) -> x > 2) emptyPredicates
        e' = addRightPredicate (\(x :: Int) -> x < 2) emptyPredicates
    withServantServer functionAPI functionAPIServer $ \burl -> do
      serverSatisfies functionAPI burl f e noOfTestCases
      serverDoesntSatisfy functionAPI burl f e' noOfTestCases

  it "allows polymorphic predicates" $ do
    let p1 x = length (show x) < 100000
        p2 x = length (show x) < 1
        e1 = addPolyPredicate (Proxy :: Proxy Show) p1 emptyPredicates
        e2 = addPolyPredicate (Proxy :: Proxy Show) p2 emptyPredicates
    withServantServer onlyReturnAPI onlyReturnAPIServer $ \burl -> do
      serverSatisfies onlyReturnAPI burl emptyPredicates e1 noOfTestCases
      serverDoesntSatisfy onlyReturnAPI burl emptyPredicates e2 noOfTestCases


  context "never500s" $ do

    it "is true for servers that don't return 500s" $ do
      withServantServer functionAPI functionAPIServer $ \burl ->
        serverSatisfies functionAPI burl emptyPredicates never500s noOfTestCases

    it "is false for servers that return 500s" $ do
      withServantServer onlyReturnAPI onlyReturnAPIServer'' $ \burl ->
        serverDoesntSatisfy onlyReturnAPI burl emptyPredicates never500s noOfTestCases

  context "onlyJsonObjects" $ do

    it "is false for servers that return top-level literals" $ do
      withServantServer onlyReturnAPI onlyReturnAPIServer $ \burl ->
        serverDoesntSatisfy onlyReturnAPI burl emptyPredicates onlyJsonObjects noOfTestCases


serverBenchmarkSpec :: Spec
serverBenchmarkSpec = describe "serverBenchmark" $ do

  it "works" $ do
    withServantServer onlyReturnAPI onlyReturnAPIServer $ \burl ->
      serverBenchmark onlyReturnAPI burl defaultBenchOptions

------------------------------------------------------------------------------
-- APIs
------------------------------------------------------------------------------

-- * OnlyReturn

type OnlyReturnAPI = Get '[JSON] Int
                :<|> Post '[JSON] String

onlyReturnAPI :: Proxy OnlyReturnAPI
onlyReturnAPI = Proxy

onlyReturnAPIServer :: IO (Server OnlyReturnAPI)
onlyReturnAPIServer = return $ return 5 :<|> return "hi"

onlyReturnAPIServer' :: IO (Server OnlyReturnAPI)
onlyReturnAPIServer' = return $ return 5 :<|> return "hia"

onlyReturnAPIServer'' :: IO (Server OnlyReturnAPI)
onlyReturnAPIServer'' = return $ error "err" :<|> return "hia"

-- * Function

type FunctionAPI = ReqBody '[JSON] String :> Post '[JSON] Int
              :<|> Header "X-abool" Bool :> Get '[JSON] (Maybe Bool)

functionAPI :: Proxy FunctionAPI
functionAPI = Proxy

functionAPIServer :: IO (Server FunctionAPI)
functionAPIServer = return $ return . length :<|> return

functionAPIServer' :: IO (Server FunctionAPI)
functionAPIServer'
  = return $ (\x -> return $ length x - 1) :<|> \x -> return (not <$> x)

-- * Stateful

type StatefulAPI = ReqBody '[JSON] String :> Post '[JSON] String
              :<|> Get '[JSON] Int

statefulAPI :: Proxy StatefulAPI
statefulAPI = Proxy

statefulAPIServer :: IO (Server StatefulAPI)
statefulAPIServer = do
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
