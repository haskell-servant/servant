{-# LANGUAGE CPP #-}
module Servant.QuickCheck.InternalSpec (spec) where


import           Control.Concurrent.MVar      (newMVar, readMVar, swapMVar)
import           Control.Exception            (SomeException)
import           Control.Monad                (replicateM)
import           Control.Monad.IO.Class       (liftIO)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as C
import           Data.Maybe                   (fromJust)
import           Prelude.Compat
import           Servant
import           Test.Hspec                   (Spec, context, describe, it, shouldBe,
                                               shouldContain)
import           Test.Hspec.Core.Spec         (Arg, Example, Result (..),
                                               defaultParams, safeEvaluateExample)
import           Test.QuickCheck.Gen          (unGen, generate)
import           Test.QuickCheck.Random       (mkQCGen)
import           Network.HTTP.Client          (queryString, path)


#if MIN_VERSION_servant(0,8,0)
import Servant.API.Internal.Test.ComprehensiveAPI (comprehensiveAPIWithoutRaw)
#else
import Servant.API.Internal.Test.ComprehensiveAPI (ComprehensiveAPI,
                                                   comprehensiveAPI)
#endif

import Servant.QuickCheck
import Servant.QuickCheck.Internal (genRequest, runGenRequest, serverDoesntSatisfy)


spec :: Spec
spec = do
  serversEqualSpec
  serverSatisfiesSpec
  isComprehensiveSpec
  onlyJsonObjectSpec
  notLongerThanSpec
  queryParamsSpec
  queryFlagsSpec
  deepPathSpec
  unbiasedGenerationSpec

serversEqualSpec :: Spec
serversEqualSpec = describe "serversEqual" $ do

  it "considers equal servers equal" $ do
    withServantServerAndContext api ctx server $ \burl1 ->
      withServantServerAndContext api ctx server $ \burl2 -> do
        serversEqual api burl1 burl2 args bodyEquality

  context "when servers are not equal" $ do
    it "provides the failing responses in the error message" $ do
      Right (Failure _ err) <- withServantServer api2 server2 $ \burl1 ->
        withServantServer api2 server3 $ \burl2 -> do
          safeEvalExample $ serversEqual api2 burl1 burl2 args bodyEquality
      show err `shouldContain` "Server equality failed"
      show err `shouldContain` "Body: 1"
      show err `shouldContain` "Body: 2"
      show err `shouldContain` "Path: /failplz"

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
      Right (Failure _ err) <- withServantServerAndContext api ctx server $ \burl -> do
        safeEvalExample $ serverSatisfies api burl args (notAllowedContainsAllowHeader <%> mempty)
      show err `shouldContain` "notAllowedContainsAllowHeader"
      show err `shouldContain` "Headers"
      show err `shouldContain` "Body"


onlyJsonObjectSpec :: Spec
onlyJsonObjectSpec = describe "onlyJsonObjects" $ do

  it "fails correctly" $ do
    Right (Failure _ err) <- withServantServerAndContext api ctx server $ \burl -> do
      safeEvalExample $ serverSatisfies (Proxy :: Proxy (Get '[JSON] Int)) burl args
        (onlyJsonObjects <%> mempty)
    show err `shouldContain` "onlyJsonObjects"

  it "accepts non-JSON endpoints" $ do
    withServantServerAndContext octetAPI ctx serverOctetAPI $ \burl ->
      serverSatisfies octetAPI burl args (onlyJsonObjects <%> mempty)

notLongerThanSpec :: Spec
notLongerThanSpec = describe "notLongerThan" $ do

  it "fails correctly" $ do
    Right (Failure _ err) <- withServantServerAndContext api ctx server $ \burl -> do
      safeEvalExample $ serverSatisfies (Proxy :: Proxy (Get '[JSON] Int)) burl args
        (notLongerThan 1 <%> mempty)
    show err `shouldContain` "notLongerThan"

  it "succeeds correctly" $ do
    withServantServerAndContext api ctx server $ \burl ->
      serverSatisfies api burl args (notLongerThan 1000000000000 <%> mempty)

isComprehensiveSpec :: Spec
isComprehensiveSpec = describe "HasGenRequest" $ do

  it "has instances for all 'servant' combinators" $ do
    let _g = genRequest comprehensiveAPIWithoutRaw
    True `shouldBe` True -- This is a type-level check

deepPathSpec :: Spec
deepPathSpec = describe "Path components" $ do

  it "are separated by slashes, without a trailing slash" $ do
    let rng = mkQCGen 0
        burl = BaseUrl Http "localhost" 80 ""
        gen = runGenRequest deepAPI
        req = (unGen gen rng 0) burl
    path req `shouldBe` ("/one/two/three")


queryParamsSpec :: Spec
queryParamsSpec = describe "QueryParams" $ do

  it "reduce to an HTTP query string correctly" $ do
    let rng = mkQCGen 0
        burl = BaseUrl Http "localhost" 80 ""
        gen = runGenRequest paramsAPI
        req = (unGen gen rng 0) burl
        qs = C.unpack $ queryString req
    qs `shouldBe` "one=_&two=_"

queryFlagsSpec :: Spec
queryFlagsSpec = describe "QueryFlags" $ do

  it "reduce to an HTTP query string correctly" $ do
    let rng = mkQCGen 0
        burl = BaseUrl Http "localhost" 80 ""
        gen = runGenRequest flagsAPI
        req = (unGen gen rng 0) burl
        qs = C.unpack $ queryString req
    qs `shouldBe` "one&two"

makeRandomRequest :: Proxy LargeAPI -> BaseUrl -> IO Integer
makeRandomRequest large burl = do
  req <- generate $ runGenRequest large
  pure $ fst . fromJust . C.readInteger . C.drop 1 . path $ req burl


unbiasedGenerationSpec :: Spec
unbiasedGenerationSpec = describe "Unbiased Generation of requests" $

  it "frequency paired with generated endpoint should be more randomly distributed" $ do
    let burl = BaseUrl Http "localhost" 80 ""
    let runs = 10000 :: Double
    someRequests <- replicateM 10000 (makeRandomRequest largeApi burl)
    let mean = (sum $ map fromIntegral someRequests) / runs
    let variancer x = let ix = fromIntegral x in (ix - mean) * (ix - mean)
    let variance = (sum $ map variancer someRequests) / runs - 1
    -- mean should be around 8.5. If this fails, we likely need more runs (or there's a bug!)
    mean > 8 `shouldBe` True
    mean < 9 `shouldBe` True
    -- Std dev is likely around 4. Variance is probably greater than 20.
    variance > 19.5 `shouldBe` True

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

type DeepAPI = "one" :> "two" :> "three":> Get '[JSON] ()

deepAPI :: Proxy DeepAPI
deepAPI = Proxy


server2 :: IO (Server API2)
server2 = return $ return 1

server3 :: IO (Server API2)
server3 = return $ return 2


largeApi :: Proxy LargeAPI
largeApi = Proxy

type LargeAPI
  =    "1" :> Get '[JSON] Int
  :<|> "2" :> Get '[JSON] Int
  :<|> "3" :> Get '[JSON] Int
  :<|> "4" :> Get '[JSON] Int
  :<|> "5" :> Get '[JSON] Int
  :<|> "6" :> Get '[JSON] Int
  :<|> "7" :> Get '[JSON] Int
  :<|> "8" :> Get '[JSON] Int
  :<|> "9" :> Get '[JSON] Int
  :<|> "10" :> Get '[JSON] Int
  :<|> "11" :> Get '[JSON] Int
  :<|> "12" :> Get '[JSON] Int
  :<|> "13" :> Get '[JSON] Int
  :<|> "14" :> Get '[JSON] Int
  :<|> "15" :> Get '[JSON] Int
  :<|> "16" :> Get '[JSON] Int


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
safeEvalExample :: (Example e, Arg e ~ ()) => e -> IO (Either SomeException Result)
safeEvalExample e = safeEvaluateExample e defaultParams ($ ()) progCallback
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
