{-# LANGUAGE CPP #-}
module Servant.QuickCheck.InternalSpec (spec) where

import           Control.Concurrent.MVar                    (newMVar, readMVar,
                                                             swapMVar)
import           Control.Monad.IO.Class                     (liftIO)
import           Prelude.Compat
import           Servant
import           Servant.API.Internal.Test.ComprehensiveAPI (comprehensiveAPI)
import           Test.Hspec                                 (Spec, context,
                                                             describe, it,
                                                             pending, shouldBe)
import           Test.Hspec.Core.Spec                       (Arg, Example,
                                                             Result (..),
                                                             defaultParams,
                                                             evaluateExample)

import           Servant.QuickCheck
import           Servant.QuickCheck.Internal                (genRequest, Failure(..), serverDoesntSatisfy)

spec :: Spec
spec = do
  serversEqualSpec
  serverSatisfiesSpec
  isComprehensiveSpec

serversEqualSpec :: Spec
serversEqualSpec = describe "serversEqual" $ do

  it "considers equal servers equal" $ do
    withServantServerAndContext api ctx server $ \burl1 ->
      withServantServerAndContext api ctx server $ \burl2 -> do
        serversEqual api burl1 burl2 args bodyEquality

  context "when servers are not equal" $ do

    it "provides the failing requests in the error message" $ do
      e <- withServantServer api2 server2 $ \burl1 ->
        withServantServer api2 server3 $ \burl2 -> do
          evalExample $ serversEqual api2 burl1 burl2 args bodyEquality
      e `shouldBe` e

    it "provides the failing requests in the error message" $ do
      Fail _ err <- withServantServer api2 server2 $ \burl1 ->
        withServantServer api2 server3 $ \burl2 -> do
          evalExample $ serversEqual api2 burl1 burl2 args bodyEquality
      print err
      let ServerEqualityFailure req _ _ = read err
      req `shouldBe` "failplz"

    {-it "provides the failing responses in the error message" $ do-}
      {-Fail _ err <- withServantServer api2 server2 $ \burl1 ->-}
        {-withServantServer api2 server3 $ \burl2 -> do-}
          {-evalExample $ serversEqual api2 burl1 burl2 args bodyEquality-}
      {-let ServerEqualityFailure _ r1 r2 = read err-}
      {-r1 `shouldBe` "1"-}
      {-r2 `shouldBe` "2"-}


serverSatisfiesSpec :: Spec
serverSatisfiesSpec = describe "serverSatisfies" $ do

  it "succeeds for true predicates" $ do
    withServantServerAndContext api ctx server $ \burl ->
      serverSatisfies api burl args (unauthorizedContainsWWWAuthenticate
                                 <%> not500
                                 <%> mempty)

  it "fails for false predicates" $ do
    withServantServerAndContext api ctx server $ \burl -> do
      serverDoesntSatisfy api burl args (onlyJsonObjects
                                     <%> getsHaveCacheControlHeader
                                     <%> headsHaveCacheControlHeader
                                     <%> notAllowedContainsAllowHeader
                                     <%> mempty)

  context "when predicates are false" $
    it "fails with informative error messages" $ pending

isComprehensiveSpec :: Spec
isComprehensiveSpec = describe "HasGenRequest" $ do

  it "has instances for all 'servant' combinators" $ do
    let _g = genRequest comprehensiveAPI
    True `shouldBe` True -- This is a type-level check


------------------------------------------------------------------------------
-- APIs
------------------------------------------------------------------------------

type API = ReqBody '[JSON] String :> Post '[JSON] String
      :<|> Get '[JSON] Int
      :<|> BasicAuth "some-realm" () :> Get '[JSON] ()

api :: Proxy API
api = Proxy

server :: IO (Server API)
server = do
    mvar <- newMVar ""
    return $ (\x -> liftIO $ swapMVar mvar x)
        :<|> (liftIO $ readMVar mvar >>= return . length)
        :<|> (const $ return ())


type API2 = "failplz" :> Get '[JSON] Int

api2 :: Proxy API2
api2 = Proxy

server2 :: IO (Server API2)
server2 = return $ return 1

server3 :: IO (Server API2)
server3 = return $ return 2

ctx :: Context '[BasicAuthCheck ()]
ctx = BasicAuthCheck (const . return $ NoSuchUser) :. EmptyContext
------------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------------

evalExample :: (Example e, Arg e ~ ()) => e -> IO Result
evalExample e = evaluateExample e defaultParams ($ ()) progCallback
  where
    progCallback _ = return ()

args :: Args
args = defaultArgs { maxSuccess = noOfTestCases }

noOfTestCases :: Int
#if LONG_TESTS
noOfTestCases = 20000
#else
noOfTestCases = 500
#endif
