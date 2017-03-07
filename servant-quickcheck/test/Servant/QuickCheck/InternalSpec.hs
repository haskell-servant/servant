{-# LANGUAGE CPP #-}
module Servant.QuickCheck.InternalSpec (spec) where

import           Control.Concurrent.MVar (newMVar, readMVar, swapMVar)
import           Control.Monad.IO.Class  (liftIO)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as C
import           Prelude.Compat
import           Servant
import           Test.Hspec              (Spec, context, describe, it, shouldBe,
                                          shouldContain)
import           Test.Hspec.Core.Spec    (Arg, Example, Result (..),
                                          defaultParams, evaluateExample)
import           Test.QuickCheck.Gen     (unGen)
import           Test.QuickCheck.Random  (mkQCGen)
import           Network.HTTP.Client     (queryString)

#if MIN_VERSION_servant(0,8,0)
import Servant.API.Internal.Test.ComprehensiveAPI (comprehensiveAPIWithoutRaw)
#else
import Servant.API.Internal.Test.ComprehensiveAPI (ComprehensiveAPI,
                                                   comprehensiveAPI)
#endif

import Servant.QuickCheck
import Servant.QuickCheck.Internal (genRequest, serverDoesntSatisfy)

spec :: Spec
spec = do
  serversEqualSpec
  serverSatisfiesSpec
  isComprehensiveSpec
  onlyJsonObjectSpec
  notLongerThanSpec
  queryParamsSpec
  queryFlagsSpec

serversEqualSpec :: Spec
serversEqualSpec = describe "serversEqual" $ do

  it "considers equal servers equal" $ do
    withServantServerAndContext api ctx server $ \burl1 ->
      withServantServerAndContext api ctx server $ \burl2 -> do
        serversEqual api burl1 burl2 args bodyEquality

  context "when servers are not equal" $ do


    it "provides the failing responses in the error message" $ do
      Fail _ err <- withServantServer api2 server2 $ \burl1 ->
        withServantServer api2 server3 $ \burl2 -> do
          evalExample $ serversEqual api2 burl1 burl2 args bodyEquality
      show err `shouldContain` "Body: 1"
      show err `shouldContain` "Body: 2"
      show err `shouldContain` "Path: failplz/"

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

  context "when predicates are false" $ do

    it "fails with informative error messages" $ do
      Fail _ err <- withServantServerAndContext api ctx server $ \burl -> do
        evalExample $ serverSatisfies api burl args (getsHaveCacheControlHeader <%> mempty)
      err `shouldContain` "getsHaveCacheControlHeader"
      err `shouldContain` "Headers"
      err `shouldContain` "Body"

onlyJsonObjectSpec :: Spec
onlyJsonObjectSpec = describe "onlyJsonObjects" $ do

  it "fails correctly" $ do
    Fail _ err <- withServantServerAndContext api ctx server $ \burl -> do
      evalExample $ serverSatisfies (Proxy :: Proxy (Get '[JSON] Int)) burl args
        (onlyJsonObjects <%> mempty)
    err `shouldContain` "onlyJsonObjects"

  it "accepts non-JSON endpoints" $ do
    withServantServerAndContext octetAPI ctx serverOctetAPI $ \burl ->
      serverSatisfies octetAPI burl args (onlyJsonObjects <%> mempty)

notLongerThanSpec :: Spec
notLongerThanSpec = describe "notLongerThan" $ do

  it "fails correctly" $ do
    Fail _ err <- withServantServerAndContext api ctx server $ \burl -> do
      evalExample $ serverSatisfies (Proxy :: Proxy (Get '[JSON] Int)) burl args
        (notLongerThan 1 <%> mempty)
    err `shouldContain` "notLongerThan"

  it "succeeds correctly" $ do
    withServantServerAndContext api ctx server $ \burl ->
      serverSatisfies api burl args (notLongerThan 1000000000000 <%> mempty)

isComprehensiveSpec :: Spec
isComprehensiveSpec = describe "HasGenRequest" $ do

  it "has instances for all 'servant' combinators" $ do
    let _g = genRequest comprehensiveAPIWithoutRaw
    True `shouldBe` True -- This is a type-level check

queryParamsSpec :: Spec
queryParamsSpec = describe "QueryParams" $ do

  it "reduce to an HTTP query string correctly" $ do
    let rng = mkQCGen 0
        burl = BaseUrl Http "localhost" 80 ""
        gen = genRequest paramsAPI
        req = (unGen gen rng 0) burl
        qs = C.unpack $ queryString req
    qs `shouldBe` "one=_&two=_"

queryFlagsSpec :: Spec
queryFlagsSpec = describe "QueryFlags" $ do

  it "reduce to an HTTP query string correctly" $ do
    let rng = mkQCGen 0
        burl = BaseUrl Http "localhost" 80 ""
        gen = genRequest flagsAPI
        req = (unGen gen rng 0) burl
        qs = C.unpack $ queryString req
    qs `shouldBe` "one=&two="

------------------------------------------------------------------------------
-- APIs
------------------------------------------------------------------------------

type API = ReqBody '[JSON] String :> Post '[JSON] String
      :<|> Get '[JSON] Int
      :<|> BasicAuth "some-realm" () :> Get '[JSON] ()

api :: Proxy API
api = Proxy

type ParamsAPI = QueryParam "one" () :> QueryParam "two" () :> Get '[JSON] ()

paramsAPI :: Proxy ParamsAPI
paramsAPI = Proxy

type FlagsAPI = QueryFlag "one" :> QueryFlag "two" :> Get '[JSON] ()

flagsAPI :: Proxy FlagsAPI
flagsAPI = Proxy


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

type OctetAPI = Get '[OctetStream] BS.ByteString

octetAPI :: Proxy OctetAPI
octetAPI = Proxy

serverOctetAPI :: IO (Server OctetAPI)
serverOctetAPI = return $ return "blah"

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
noOfTestCases = 1000
#endif

#if !MIN_VERSION_servant(0,8,0)
comprehensiveAPIWithoutRaw :: Proxy ComprehensiveAPI
comprehensiveAPIWithoutRaw = comprehensiveAPI
#endif
