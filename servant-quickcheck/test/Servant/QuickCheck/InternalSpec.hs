{-# LANGUAGE CPP #-}
module Servant.QuickCheck.InternalSpec (spec) where


import           Control.Concurrent.MVar (newMVar, readMVar, swapMVar)
import           Control.Exception       (SomeException)
import           Control.Monad           (replicateM)
import           Control.Monad.IO.Class  (liftIO)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as C
import           Data.Maybe              (fromJust)
import           Network.HTTP.Client     (path, queryString)
import           Prelude.Compat
import           Servant
import           Servant.HTML.Blaze      (HTML)
import qualified Text.Blaze.Html         as Blaze
import qualified Text.Blaze.Html5        as Blaze5
import           Test.Hspec              (Spec, context, describe, it, shouldBe,
                                          shouldContain)
import           Test.Hspec.Core.Spec    (Arg, Example, Result (..), ResultStatus (..),
                                          defaultParams, safeEvaluateExample)
import           Test.QuickCheck.Gen     (generate, unGen)
import           Test.QuickCheck.Random  (mkQCGen)


#if MIN_VERSION_servant(0,8,0)
import Servant.API.Internal.Test.ComprehensiveAPI (comprehensiveAPIWithoutRaw)
#else
import Servant.API.Internal.Test.ComprehensiveAPI (ComprehensiveAPI,
                                                   comprehensiveAPI)
#endif

import Servant.QuickCheck
import Servant.QuickCheck.Internal (genRequest, runGenRequest,
                                    serverDoesntSatisfy)

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
  htmlDocTypesSpec
  unbiasedGenerationSpec

serversEqualSpec :: Spec
serversEqualSpec = describe "serversEqual" $ do

  it "considers equal servers equal" $ do
    withServantServerAndContext api ctx server $ \burl1 ->
      withServantServerAndContext api ctx server $ \burl2 -> do
        serversEqual api burl1 burl2 args bodyEquality

  context "when servers are not equal" $ do
    it "provides the failing responses in the error message" $ do
      FailedWith err <- withServantServer api2 server2 $ \burl1 ->
        withServantServer api2 server3 $ \burl2 -> do
          evalExample $ serversEqual api2 burl1 burl2 args bodyEquality
      show err `shouldContain` "Server equality failed"
      show err `shouldContain` "Body: 1"
      show err `shouldContain` "Body: 2"
      show err `shouldContain` "Path: /failplz"

  context "when JSON is equal but looks a bit different as a ByteString" $ do

    it "sanity check: different whitespace same JSON objects bodyEquality fails" $ do
      FailedWith err <- withServantServer jsonApi jsonServer1 $ \burl1 ->
        withServantServer jsonApi jsonServer2 $ \burl2 -> do
          evalExample $ serversEqual jsonApi burl1 burl2 args bodyEquality
      show err `shouldContain` "Server equality failed"

    it "jsonEquality considers equal JSON apis equal regardless of key ordering or whitespace" $ do
      withServantServerAndContext jsonApi ctx jsonServer1 $ \burl1 ->
        withServantServerAndContext jsonApi ctx jsonServer2 $ \burl2 ->
          serversEqual jsonApi burl1 burl2 args jsonEquality

    it "sees when JSON apis are not equal because any value is different" $ do
      FailedWith err <- withServantServer jsonApi jsonServer2 $ \burl1 ->
        withServantServer jsonApi jsonServer3 $ \burl2 -> do
          evalExample $ serversEqual jsonApi burl1 burl2 args jsonEquality
      show err `shouldContain` "Server equality failed"
      show err `shouldContain` "Path: /jsonComparison"

    it "sees when JSON apis are not equal due to different keys but same values" $ do
      FailedWith err <- withServantServer jsonApi jsonServer2 $ \burl1 ->
        withServantServer jsonApi jsonServer4 $ \burl2 -> do
          evalExample $ serversEqual jsonApi burl1 burl2 args jsonEquality
      show err `shouldContain` "Server equality failed"
      show err `shouldContain` "Path: /jsonComparison"


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
      FailedWith err <- withServantServerAndContext api ctx server $ \burl -> do
        evalExample $ serverSatisfies api burl args (notAllowedContainsAllowHeader <%> mempty)
      show err `shouldContain` "notAllowedContainsAllowHeader"
      show err `shouldContain` "Headers"
      show err `shouldContain` "Body"


onlyJsonObjectSpec :: Spec
onlyJsonObjectSpec = describe "onlyJsonObjects" $ do

  it "fails correctly" $ do
    FailedWith err <- withServantServerAndContext api ctx server $ \burl -> do
      evalExample $ serverSatisfies (Proxy :: Proxy (Get '[JSON] Int)) burl args
        (onlyJsonObjects <%> mempty)
    show err `shouldContain` "onlyJsonObjects"

  it "accepts non-JSON endpoints" $ do
    withServantServerAndContext octetAPI ctx serverOctetAPI $ \burl ->
      serverSatisfies octetAPI burl args (onlyJsonObjects <%> mempty)

  it "does not fail when there is no content-type" $ do
    withServantServerAndContext api2 ctx serverFailing $ \burl ->
        serverSatisfies api2 burl args (onlyJsonObjects <%> mempty)

notLongerThanSpec :: Spec
notLongerThanSpec = describe "notLongerThan" $ do

  it "fails correctly" $ do
    FailedWith err <- withServantServerAndContext api ctx server $ \burl -> do
      evalExample $ serverSatisfies (Proxy :: Proxy (Get '[JSON] Int)) burl args
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

htmlDocTypesSpec :: Spec
htmlDocTypesSpec = describe "HtmlDocTypes" $ do

    it "fails HTML without doctype correctly" $ do
      err <- withServantServerAndContext docTypeApi ctx noDocTypeServer $ \burl -> do
        evalExample $ serverSatisfies docTypeApi burl args
          (htmlIncludesDoctype <%> mempty)
      show err `shouldContain` "htmlIncludesDoctype"

    it "passes HTML with a doctype at start" $ do
      withServantServerAndContext docTypeApi ctx docTypeServer $ \burl ->
        serverSatisfies docTypeApi burl args (htmlIncludesDoctype <%> mempty)

    it "accepts json endpoints and passes over them in silence" $ do
      withServantServerAndContext api ctx server $ \burl -> do
        serverSatisfies (Proxy :: Proxy (Get '[JSON] Int)) burl args
          (htmlIncludesDoctype <%> mempty)


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

serverFailing :: IO (Server API2)
serverFailing = return . throwError $ err405

-- With Doctypes
type HtmlDoctype = Get '[HTML] Blaze.Html

docTypeApi :: Proxy HtmlDoctype
docTypeApi = Proxy

docTypeServer :: IO (Server HtmlDoctype)
docTypeServer = pure $ pure $ Blaze5.docTypeHtml $ Blaze5.span "Hello Test!"

noDocTypeServer :: IO (Server HtmlDoctype)
noDocTypeServer = pure $ pure $ Blaze.text "Hello Test!"


-- Api for unbiased generation of requests tests
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

type JsonApi = "jsonComparison" :> Get '[OctetStream] BS.ByteString

jsonApi :: Proxy JsonApi
jsonApi = Proxy

jsonServer1 :: IO (Server JsonApi)
jsonServer1 = return $ return "{ \"b\": [\"b\"], \"a\": 1 }"  -- whitespace, ordering different

jsonServer2 :: IO (Server JsonApi)
jsonServer2 = return $ return "{\"a\": 1,\"b\":[\"b\"]}"

jsonServer3 :: IO (Server JsonApi)
jsonServer3 = return $ return "{\"a\": 2, \"b\": [\"b\"]}"

jsonServer4 :: IO (Server JsonApi)
jsonServer4 = return $ return "{\"c\": 1, \"d\": [\"b\"]}"


ctx :: Context '[BasicAuthCheck ()]
ctx = BasicAuthCheck (const . return $ NoSuchUser) :. EmptyContext
------------------------------------------------------------------------------
-- Utils
------------------------------------------------------------------------------
evalExample :: (Example e, Arg e ~ ()) => e -> IO EvalResult
evalExample e = do
  r <- safeEvaluateExample e defaultParams ($ ()) progCallback
  case resultStatus r of
    Success          -> return $ AllGood
    Failure _ reason -> return $ FailedWith $ show reason
    Pending {}       -> error "should not happen"
  where
    progCallback _ = return ()

data EvalResult
  = AnException SomeException
  | AllGood
  | FailedWith String
  deriving (Show)


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
