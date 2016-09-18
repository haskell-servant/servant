{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Servant.ServerSpec where

import           Control.Monad              (forM_, when, unless)
import           Control.Monad.Trans.Except (throwE)
import           Data.Aeson                 (FromJSON, ToJSON, decode', encode)
import qualified Data.ByteString.Base64     as Base64
import           Data.Char                  (toUpper)
import           Data.Monoid
import           Data.Proxy                 (Proxy (Proxy))
import           Data.String                (fromString)
import           Data.String.Conversions    (cs)
import qualified Data.Text                  as T
import           GHC.Generics               (Generic)
import           Network.HTTP.Types         (Status (..), hAccept, hContentType,
                                             methodDelete, methodGet,
                                             methodHead, methodPatch,
                                             methodPost, methodPut, ok200,
                                             imATeaPot418,
                                             parseQuery)
import           Network.Wai                (Application, Request, requestHeaders, pathInfo,
                                             queryString, rawQueryString,
                                             responseLBS)
import           Network.Wai.Test           (defaultRequest, request,
                                             runSession, simpleBody,
                                             simpleHeaders, simpleStatus)
import           Servant.API                ((:<|>) (..), (:>), AuthProtect,
                                             BasicAuth, BasicAuthData(BasicAuthData),
                                             Capture, CaptureAll, Delete, Get, Header (..),
                                             Headers, HttpVersion,
                                             IsSecure (..), JSON,
                                             NoContent (..), Patch, PlainText,
                                             Post, Put,
                                             QueryFlag, QueryParam, QueryParams,
                                             Raw, RemoteHost, ReqBody,
                                             StdMethod (..), Verb, addHeader,
                                             FTime(..))
import           Servant.API.Internal.Test.ComprehensiveAPI
import           Servant.Server             (Server, Handler, err401, err403,
                                             err404, serve, serveWithContext,
                                             Context((:.), EmptyContext))
import           Test.Hspec                 (Spec, context, describe, it,
                                             shouldBe, shouldContain)
import qualified Test.Hspec.Wai             as THW
import           Test.Hspec.Wai             (get, liftIO, matchHeaders,
                                             matchStatus, shouldRespondWith,
                                             with, (<:>))

import           Servant.Server.Internal.BasicAuth (BasicAuthCheck(BasicAuthCheck),
                                                    BasicAuthResult(Authorized,Unauthorized))
import           Servant.Server.Experimental.Auth
                                            (AuthHandler, AuthServerData,
                                             mkAuthHandler)
import           Servant.Server.Internal.Context
                                            (NamedContext(..))
import           Data.Time.Calendar         (Day, fromGregorian)
import           Data.Time.LocalTime        (LocalTime(..), hoursToTimeZone,
                                             localTimeToUTC, makeTimeOfDayValid)
import           Data.Time.Clock            (UTCTime)
import           Data.ByteString.Lazy       (ByteString)
-- * comprehensive api test

-- This declaration simply checks that all instances are in place.
_ = serveWithContext comprehensiveAPI comprehensiveApiContext

comprehensiveApiContext :: Context '[NamedContext "foo" '[]]
comprehensiveApiContext = NamedContext EmptyContext :. EmptyContext

-- * Specs

spec :: Spec
spec = do
  verbSpec
  captureSpec
  captureTimeSpec
  queryParamSpec
  reqBodySpec
  headerSpec
  rawSpec
  alternativeSpec
  responseHeadersSpec
  miscCombinatorSpec
  basicAuthSpec
  genAuthSpec

------------------------------------------------------------------------------
-- * verbSpec {{{
------------------------------------------------------------------------------

type VerbApi method status
    =                Verb method status '[JSON] Person
 :<|> "noContent" :> Verb method status '[JSON] NoContent
 :<|> "header"    :> Verb method status '[JSON] (Headers '[Header "H" Int] Person)
 :<|> "headerNC"  :> Verb method status '[JSON] (Headers '[Header "H" Int] NoContent)
 :<|> "accept"    :> (    Verb method status '[JSON] Person
                     :<|> Verb method status '[PlainText] String
                     )

verbSpec :: Spec
verbSpec = describe "Servant.API.Verb" $ do
  let server :: Server (VerbApi method status)
      server = return alice
          :<|> return NoContent
          :<|> return (addHeader 5 alice)
          :<|> return (addHeader 10 NoContent)
          :<|> (return alice :<|> return "B")
      get200     = Proxy :: Proxy (VerbApi 'GET 200)
      post210    = Proxy :: Proxy (VerbApi 'POST 210)
      put203     = Proxy :: Proxy (VerbApi 'PUT 203)
      delete280  = Proxy :: Proxy (VerbApi 'DELETE 280)
      patch214   = Proxy :: Proxy (VerbApi 'PATCH 214)
      wrongMethod m = if m == methodPatch then methodPost else methodPatch
      test desc api method (status :: Int) = context desc $

        with (return $ serve api server) $ do

          -- HEAD and 214/215 need not return bodies
          unless (status `elem` [214, 215] || method == methodHead) $
            it "returns the person" $ do
              response <- THW.request method "/" [] ""
              liftIO $ statusCode (simpleStatus response) `shouldBe` status
              liftIO $ decode' (simpleBody response) `shouldBe` Just alice

          it "returns no content on NoContent" $ do
              response <- THW.request method "/noContent" [] ""
              liftIO $ statusCode (simpleStatus response) `shouldBe` status
              liftIO $ simpleBody response `shouldBe` ""

          -- HEAD should not return body
          when (method == methodHead) $
            it "HEAD returns no content body" $ do
              response <- THW.request method "/" [] ""
              liftIO $ simpleBody response `shouldBe` ""

          it "throws 405 on wrong method " $ do
            THW.request (wrongMethod method) "/" [] ""
              `shouldRespondWith` 405

          it "returns headers" $ do
            response1 <- THW.request method "/header" [] ""
            liftIO $ statusCode (simpleStatus response1) `shouldBe` status
            liftIO $ simpleHeaders response1 `shouldContain` [("H", "5")]

            response2 <- THW.request method "/header" [] ""
            liftIO $ statusCode (simpleStatus response2) `shouldBe` status
            liftIO $ simpleHeaders response2 `shouldContain` [("H", "5")]

          it "handles trailing '/' gracefully" $ do
            response <- THW.request method "/headerNC/" [] ""
            liftIO $ statusCode (simpleStatus response) `shouldBe` status

          it "returns 406 if the Accept header is not supported" $ do
            THW.request method "" [(hAccept, "crazy/mime")] ""
              `shouldRespondWith` 406

          it "responds if the Accept header is supported" $ do
            response <- THW.request method ""
               [(hAccept, "application/json")] ""
            liftIO $ statusCode (simpleStatus response) `shouldBe` status

          unless (status `elem` [214, 215] || method == methodHead) $
            it "allows modular specification of supported content types" $ do
              response <- THW.request method "/accept" [(hAccept, "text/plain")] ""
              liftIO $ statusCode (simpleStatus response) `shouldBe` status
              liftIO $ simpleBody response `shouldBe` "B"

          it "sets the Content-Type header" $ do
            response <- THW.request method "" [] ""
            liftIO $ simpleHeaders response `shouldContain`
              [("Content-Type", "application/json")]

  test "GET 200" get200 methodGet 200
  test "POST 210" post210 methodPost 210
  test "PUT 203" put203 methodPut 203
  test "DELETE 280" delete280 methodDelete 280
  test "PATCH 214" patch214 methodPatch 214
  test "GET 200 with HEAD" get200 methodHead 200

-- }}}
------------------------------------------------------------------------------
-- * captureSpec {{{
------------------------------------------------------------------------------

type CaptureApi = Capture "legs" Integer :> Get '[JSON] Animal
captureApi :: Proxy CaptureApi
captureApi = Proxy
captureServer :: Integer -> Handler Animal
captureServer legs = case legs of
  4 -> return jerry
  2 -> return tweety
  _ -> throwE err404

captureSpec :: Spec
captureSpec = do
  describe "Servant.API.Capture" $ do
    with (return (serve captureApi captureServer)) $ do

      it "can capture parts of the 'pathInfo'" $ do
        response <- get "/2"
        liftIO $ decode' (simpleBody response) `shouldBe` Just tweety

      it "returns 400 if the decoding fails" $ do
        get "/notAnInt" `shouldRespondWith` 400

    with (return (serve
        (Proxy :: Proxy (Capture "captured" String :> Raw))
        (\ "captured" request_ respond ->
            respond $ responseLBS ok200 [] (cs $ show $ pathInfo request_)))) $ do
      it "strips the captured path snippet from pathInfo" $ do
        get "/captured/foo" `shouldRespondWith` (fromString (show ["foo" :: String]))

-- }}}

------------------------------------------------------------------------------
-- * captureAllSpec {{{
------------------------------------------------------------------------------

type CaptureAllApi = CaptureAll "legs" Integer :> Get '[JSON] Animal
captureAllApi :: Proxy CaptureAllApi
captureAllApi = Proxy
captureAllServer :: [Integer] -> Handler Animal
captureAllServer legs = case sum legs of
  4 -> return jerry
  2 -> return tweety
  0 -> return beholder
  _ -> throwE err404

captureAllSpec :: Spec
captureAllSpec = do
  describe "Servant.API.CaptureAll" $ do
    with (return (serve captureAllApi captureAllServer)) $ do

      it "can capture a single element of the 'pathInfo'" $ do
        response <- get "/2"
        liftIO $ decode' (simpleBody response) `shouldBe` Just tweety

      it "can capture multiple elements of the 'pathInfo'" $ do
        response <- get "/2/2"
        liftIO $ decode' (simpleBody response) `shouldBe` Just jerry

      it "can capture arbitrarily many elements of the 'pathInfo'" $ do
        response <- get "/1/1/0/1/0/1"
        liftIO $ decode' (simpleBody response) `shouldBe` Just jerry

      it "can capture when there are no elements in 'pathInfo'" $ do
        response <- get "/"
        liftIO $ decode' (simpleBody response) `shouldBe` Just beholder

      it "returns 400 if the decoding fails" $ do
        get "/notAnInt" `shouldRespondWith` 400

      it "returns 400 if the decoding fails, regardless of which element" $ do
        get "/1/0/0/notAnInt/3/" `shouldRespondWith` 400

      it "returns 400 if the decoding fails, even when it's multiple elements" $ do
        get "/1/0/0/notAnInt/3/orange/" `shouldRespondWith` 400

    with (return (serve
        (Proxy :: Proxy (CaptureAll "segments" String :> Raw))
        (\ _captured request_ respond ->
            respond $ responseLBS ok200 [] (cs $ show $ pathInfo request_)))) $ do
      it "consumes everything from pathInfo" $ do
        get "/captured/foo/bar/baz" `shouldRespondWith` (fromString (show ([] :: [Int])))

-- }}}
------------------------------------------------------------------------------
-- * timeCaptureSpec {{{
------------------------------------------------------------------------------

type TimeFormatWSpace = "%Y-%m-%d %H:%M:%S%Z"

type CaptureTimeApi = (Capture "date" (FTime "%Y-%m-%d" Day) :> Get '[PlainText] String)
                      :<|>
                      ("datetime" :> Capture "datetime" (FTime TimeFormatWSpace UTCTime) :> Get '[PlainText] String)
captureTimeApi :: Proxy CaptureTimeApi
captureTimeApi = Proxy
captureDateServer :: FTime "%Y-%m-%d" Day -> Handler String
captureDateServer = return . show
captureDateTimeServer :: FTime TimeFormatWSpace UTCTime -> Handler String
captureDateTimeServer = return . show



captureTimeSpec :: Spec
captureTimeSpec = do
  describe "Servant.API.Time(CaptureTime)" $ do
    with (return (serve captureTimeApi (captureDateServer :<|> captureDateTimeServer))) $ do

      it "can capture parts of the 'pathInfo' (date only)" $ do
        response <- get "/2015-12-02"
        liftIO $ simpleBody response `shouldBe` (fromString . show $ fromGregorian 2015 12 2 :: ByteString)

      it "can capture parts of the 'pathInfo' (date and time with a space)" $ do
        let day       =  fromGregorian 2015 12 2
            Just time =  makeTimeOfDayValid 12 34 56
            tz        =  hoursToTimeZone 10
            utcT      =  localTimeToUTC tz (LocalTime day time)
            ftime     :: FTime TimeFormatWSpace UTCTime
            ftime     =  FTime utcT
        response <- get "/datetime/2015-12-02%2012:34:56+1000"
        liftIO $ simpleBody response `shouldBe` (fromString . show $ ftime)


      it "returns 404 if the decoding fails" $ do
        get "/notAnInt" `shouldRespondWith` 404

    with (return (serve
        (Proxy :: Proxy (Capture "datetime" (FTime TimeFormatWSpace UTCTime) :> Raw))
        (\ (FTime _day) request_ respond ->
            respond $ responseLBS ok200 [] (cs $ show $ pathInfo request_)))) $ do
      it "strips the captured path snippet from pathInfo" $ do
        get "/2015-12-02%2012:34:56+1000/foo" `shouldRespondWith` (fromString (show ["foo" :: String]))

-- }}}
------------------------------------------------------------------------------
-- * queryParamSpec {{{
------------------------------------------------------------------------------

type QueryParamApi = QueryParam "name" String :> Get '[JSON] Person
                :<|> "a" :> QueryParams "names" String :> Get '[JSON] Person
                :<|> "b" :> QueryFlag "capitalize" :> Get '[JSON] Person

queryParamApi :: Proxy QueryParamApi
queryParamApi = Proxy

qpServer :: Server QueryParamApi
qpServer = queryParamServer :<|> qpNames :<|> qpCapitalize

  where qpNames (_:name2:_) = return alice { name = name2 }
        qpNames _           = return alice

        qpCapitalize False = return alice
        qpCapitalize True  = return alice { name = map toUpper (name alice) }

        queryParamServer (Just name_) = return alice{name = name_}
        queryParamServer Nothing = return alice

queryParamSpec :: Spec
queryParamSpec = do
  describe "Servant.API.QueryParam" $ do
      it "allows retrieving simple GET parameters" $
        (flip runSession) (serve queryParamApi qpServer) $ do
          let params1 = "?name=bob"
          response1 <- Network.Wai.Test.request defaultRequest{
            rawQueryString = params1,
            queryString = parseQuery params1
           }
          liftIO $ do
            decode' (simpleBody response1) `shouldBe` Just alice{
              name = "bob"
             }

      it "allows retrieving lists in GET parameters" $
        (flip runSession) (serve queryParamApi qpServer) $ do
          let params2 = "?names[]=bob&names[]=john"
          response2 <- Network.Wai.Test.request defaultRequest{
            rawQueryString = params2,
            queryString = parseQuery params2,
            pathInfo = ["a"]
           }
          liftIO $
            decode' (simpleBody response2) `shouldBe` Just alice{
              name = "john"
             }


      it "allows retrieving value-less GET parameters" $
        (flip runSession) (serve queryParamApi qpServer) $ do
          let params3 = "?capitalize"
          response3 <- Network.Wai.Test.request defaultRequest{
            rawQueryString = params3,
            queryString = parseQuery params3,
            pathInfo = ["b"]
           }
          liftIO $
            decode' (simpleBody response3) `shouldBe` Just alice{
              name = "ALICE"
             }

          let params3' = "?capitalize="
          response3' <- Network.Wai.Test.request defaultRequest{
            rawQueryString = params3',
            queryString = parseQuery params3',
            pathInfo = ["b"]
           }
          liftIO $
            decode' (simpleBody response3') `shouldBe` Just alice{
              name = "ALICE"
             }

          let params3'' = "?unknown="
          response3'' <- Network.Wai.Test.request defaultRequest{
            rawQueryString = params3'',
            queryString = parseQuery params3'',
            pathInfo = ["b"]
           }
          liftIO $
            decode' (simpleBody response3'') `shouldBe` Just alice{
              name = "Alice"
             }

-- }}}
------------------------------------------------------------------------------
-- * reqBodySpec {{{
------------------------------------------------------------------------------
type ReqBodyApi = ReqBody '[JSON] Person :> Post '[JSON] Person
           :<|> "blah" :> ReqBody '[JSON] Person :> Put '[JSON] Integer

reqBodyApi :: Proxy ReqBodyApi
reqBodyApi = Proxy

reqBodySpec :: Spec
reqBodySpec = describe "Servant.API.ReqBody" $ do

  let server :: Server ReqBodyApi
      server = return :<|> return . age
      mkReq method x = THW.request method x
         [(hContentType, "application/json;charset=utf-8")]

  with (return $ serve reqBodyApi server) $ do

    it "passes the argument to the handler" $ do
      response <- mkReq methodPost "" (encode alice)
      liftIO $ decode' (simpleBody response) `shouldBe` Just alice

    it "rejects invalid request bodies with status 400" $ do
      mkReq methodPut "/blah" "some invalid body" `shouldRespondWith` 400

    it "responds with 415 if the request body media type is unsupported" $ do
      THW.request methodPost "/"
        [(hContentType, "application/nonsense")] "" `shouldRespondWith` 415

-- }}}
------------------------------------------------------------------------------
-- * headerSpec {{{
------------------------------------------------------------------------------

type HeaderApi a = Header "MyHeader" a :> Delete '[JSON] NoContent
headerApi :: Proxy (HeaderApi a)
headerApi = Proxy

headerSpec :: Spec
headerSpec = describe "Servant.API.Header" $ do

    let expectsInt :: Maybe Int -> Handler NoContent
        expectsInt (Just x) = do
          when (x /= 5) $ error "Expected 5"
          return NoContent
        expectsInt Nothing  = error "Expected an int"

    let expectsString :: Maybe String -> Handler NoContent
        expectsString (Just x) = do
          when (x /= "more from you") $ error "Expected more from you"
          return NoContent
        expectsString Nothing  = error "Expected a string"

    with (return (serve headerApi expectsInt)) $ do
        let delete' x = THW.request methodDelete x [("MyHeader", "5")]

        it "passes the header to the handler (Int)" $
            delete' "/" "" `shouldRespondWith` 200

    with (return (serve headerApi expectsString)) $ do
        let delete' x = THW.request methodDelete x [("MyHeader", "more from you")]

        it "passes the header to the handler (String)" $
            delete' "/" "" `shouldRespondWith` 200

-- }}}
------------------------------------------------------------------------------
-- * rawSpec {{{
------------------------------------------------------------------------------

type RawApi = "foo" :> Raw

rawApi :: Proxy RawApi
rawApi = Proxy

rawApplication :: Show a => (Request -> a) -> Application
rawApplication f request_ respond = respond $ responseLBS ok200 []
    (cs $ show $ f request_)

rawSpec :: Spec
rawSpec = do
  describe "Servant.API.Raw" $ do
    it "runs applications" $ do
      (flip runSession) (serve rawApi (rawApplication (const (42 :: Integer)))) $ do
        response <- Network.Wai.Test.request defaultRequest{
          pathInfo = ["foo"]
         }
        liftIO $ do
          simpleBody response `shouldBe` "42"

    it "gets the pathInfo modified" $ do
      (flip runSession) (serve rawApi (rawApplication pathInfo)) $ do
        response <- Network.Wai.Test.request defaultRequest{
          pathInfo = ["foo", "bar"]
         }
        liftIO $ do
          simpleBody response `shouldBe` cs (show ["bar" :: String])

-- }}}
------------------------------------------------------------------------------
-- * alternativeSpec {{{
------------------------------------------------------------------------------
type AlternativeApi =
       "foo" :> Get '[JSON] Person
  :<|> "bar" :> Get '[JSON] Animal
  :<|> "foo" :> Get '[PlainText] T.Text
  :<|> "bar" :> Post '[JSON] Animal
  :<|> "bar" :> Put '[JSON] Animal
  :<|> "bar" :> Delete '[JSON] NoContent

alternativeApi :: Proxy AlternativeApi
alternativeApi = Proxy

alternativeServer :: Server AlternativeApi
alternativeServer =
       return alice
  :<|> return jerry
  :<|> return "a string"
  :<|> return jerry
  :<|> return jerry
  :<|> return NoContent

alternativeSpec :: Spec
alternativeSpec = do
  describe "Servant.API.Alternative" $ do
    with (return $ serve alternativeApi alternativeServer) $ do

      it "unions endpoints" $ do
        response <- get "/foo"
        liftIO $ do
          decode' (simpleBody response) `shouldBe`
            Just alice
        response_ <- get "/bar"
        liftIO $ do
          decode' (simpleBody response_) `shouldBe`
            Just jerry

      it "checks all endpoints before returning 415" $ do
        get "/foo" `shouldRespondWith` 200

      it "returns 404 if the path does not exist" $ do
        get "/nonexistent" `shouldRespondWith` 404
-- }}}
------------------------------------------------------------------------------
-- * responseHeaderSpec {{{
------------------------------------------------------------------------------
type ResponseHeadersApi =
       Get   '[JSON] (Headers '[Header "H1" Int, Header "H2" String] String)
  :<|> Post  '[JSON] (Headers '[Header "H1" Int, Header "H2" String] String)
  :<|> Put   '[JSON] (Headers '[Header "H1" Int, Header "H2" String] String)
  :<|> Patch '[JSON] (Headers '[Header "H1" Int, Header "H2" String] String)


responseHeadersServer :: Server ResponseHeadersApi
responseHeadersServer = let h = return $ addHeader 5 $ addHeader "kilroy" "hi"
  in h :<|> h :<|> h :<|> h


responseHeadersSpec :: Spec
responseHeadersSpec = describe "ResponseHeaders" $ do
  with (return $ serve (Proxy :: Proxy ResponseHeadersApi) responseHeadersServer) $ do

    let methods = [methodGet, methodPost, methodPut, methodPatch]

    it "includes the headers in the response" $
      forM_ methods $ \method ->
        THW.request method "/" [] ""
          `shouldRespondWith` "\"hi\""{ matchHeaders = ["H1" <:> "5", "H2" <:> "kilroy"]
                                      , matchStatus  = 200
                                      }

    it "responds with not found for non-existent endpoints" $
      forM_ methods $ \method ->
        THW.request method "blahblah" [] ""
          `shouldRespondWith` 404

    it "returns 406 if the Accept header is not supported" $
      forM_ methods $ \method ->
        THW.request method "" [(hAccept, "crazy/mime")] ""
          `shouldRespondWith` 406

-- }}}
------------------------------------------------------------------------------
-- * miscCombinatorSpec {{{
------------------------------------------------------------------------------
type MiscCombinatorsAPI
  =    "version" :> HttpVersion :> Get '[JSON] String
  :<|> "secure"  :> IsSecure :> Get '[JSON] String
  :<|> "host"    :> RemoteHost :> Get '[JSON] String

miscApi :: Proxy MiscCombinatorsAPI
miscApi = Proxy

miscServ :: Server MiscCombinatorsAPI
miscServ = versionHandler
      :<|> secureHandler
      :<|> hostHandler

  where versionHandler = return . show
        secureHandler Secure = return "secure"
        secureHandler NotSecure = return "not secure"
        hostHandler = return . show

miscCombinatorSpec :: Spec
miscCombinatorSpec = with (return $ serve miscApi miscServ) $
  describe "Misc. combinators for request inspection" $ do
    it "Successfully gets the HTTP version specified in the request" $
      go "/version" "\"HTTP/1.0\""

    it "Checks that hspec-wai uses HTTP, not HTTPS" $
      go "/secure" "\"not secure\""

    it "Checks that hspec-wai issues request from 0.0.0.0" $
      go "/host" "\"0.0.0.0:0\""

  where go path res = Test.Hspec.Wai.get path `shouldRespondWith` res

-- }}}
------------------------------------------------------------------------------
-- * Basic Authentication {{{
------------------------------------------------------------------------------

type BasicAuthAPI =
       BasicAuth "foo" () :> "basic" :> Get '[JSON] Animal
  :<|> Raw

basicAuthApi :: Proxy BasicAuthAPI
basicAuthApi = Proxy

basicAuthServer :: Server BasicAuthAPI
basicAuthServer =
  const (return jerry) :<|>
  (\ _ respond -> respond $ responseLBS imATeaPot418 [] "")

basicAuthContext :: Context '[ BasicAuthCheck () ]
basicAuthContext =
  let basicHandler = BasicAuthCheck $ \(BasicAuthData usr pass) ->
        if usr == "servant" && pass == "server"
          then return (Authorized ())
          else return Unauthorized
  in basicHandler :. EmptyContext

basicAuthSpec :: Spec
basicAuthSpec = do
  describe "Servant.API.BasicAuth" $ do
    with (return (serveWithContext basicAuthApi basicAuthContext basicAuthServer)) $ do

      context "Basic Authentication" $ do
        let basicAuthHeaders user password =
              [("Authorization", "Basic " <> Base64.encode (user <> ":" <> password))]
        it "returns 401 when no credentials given" $ do
          get "/basic" `shouldRespondWith` 401

        it "returns 403 when invalid credentials given" $ do
          THW.request methodGet "/basic" (basicAuthHeaders "servant" "wrong") ""
            `shouldRespondWith` 403

        it "returns 200 with the right password" $ do
          THW.request methodGet "/basic" (basicAuthHeaders "servant" "server") ""
            `shouldRespondWith` 200

        it "plays nice with subsequent Raw endpoints" $ do
          get "/foo" `shouldRespondWith` 418

-- }}}
------------------------------------------------------------------------------
-- * General Authentication {{{
------------------------------------------------------------------------------

type GenAuthAPI = AuthProtect "auth" :> "auth" :> Get '[JSON] Animal
             :<|> Raw

genAuthApi :: Proxy GenAuthAPI
genAuthApi = Proxy

genAuthServer :: Server GenAuthAPI
genAuthServer = const (return tweety)
           :<|> (\ _ respond -> respond $ responseLBS imATeaPot418 [] "")

type instance AuthServerData (AuthProtect "auth") = ()

genAuthContext :: Context '[AuthHandler Request ()]
genAuthContext =
  let authHandler = \req -> case lookup "Auth" (requestHeaders req) of
        Just "secret" -> return ()
        Just _ -> throwE err403
        Nothing -> throwE err401
  in mkAuthHandler authHandler :. EmptyContext

genAuthSpec :: Spec
genAuthSpec = do
  describe "Servant.API.Auth" $ do
    with (return (serveWithContext genAuthApi genAuthContext genAuthServer)) $ do

      context "Custom Auth Protection" $ do
        it "returns 401 when missing headers" $ do
          get "/auth" `shouldRespondWith` 401

        it "returns 403 on wrong passwords" $ do
          THW.request methodGet "/auth" [("Auth","wrong")] "" `shouldRespondWith` 403

        it "returns 200 with the right header" $ do
          THW.request methodGet "/auth" [("Auth","secret")] "" `shouldRespondWith` 200

        it "plays nice with subsequent Raw endpoints" $ do
          get "/foo" `shouldRespondWith` 418

-- }}}
------------------------------------------------------------------------------
-- * Test data types {{{
------------------------------------------------------------------------------

data Person = Person {
  name :: String,
  age  :: Integer
 }
  deriving (Eq, Show, Generic)

instance ToJSON Person
instance FromJSON Person

alice :: Person
alice = Person "Alice" 42

data Animal = Animal {
  species      :: String,
  numberOfLegs :: Integer
 }
  deriving (Eq, Show, Generic)

instance ToJSON Animal
instance FromJSON Animal

jerry :: Animal
jerry = Animal "Mouse" 4

tweety :: Animal
tweety = Animal "Bird" 2

beholder :: Animal
beholder = Animal "Beholder" 0
-- }}}
