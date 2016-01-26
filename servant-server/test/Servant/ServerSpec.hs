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

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative        ((<$>))
#endif
import           Control.Monad              (forM_, when, unless)
import           Control.Monad.Trans.Except (ExceptT, throwE)
import           Data.Aeson                 (FromJSON, ToJSON, decode', encode)
import           Data.ByteString.Conversion ()
import           Data.Char                  (toUpper)
import           Data.Proxy                 (Proxy (Proxy))
import           Data.String                (fromString)
import           Data.String.Conversions    (cs)
import qualified Data.Text                  as T
import           GHC.Generics               (Generic)
import           Network.HTTP.Types         (Status (..), hAccept, hContentType,
                                             methodDelete, methodGet,
                                             methodHead, methodPatch,
                                             methodPost, methodPut, ok200,
                                             parseQuery)
import           Network.Wai                (Application, Request, requestHeaders, pathInfo,
                                             queryString, rawQueryString,
                                             responseBuilder, responseLBS)
import           Network.Wai.Internal       (Response (ResponseBuilder))
import           Network.Wai.Test           (defaultRequest, request,
                                             runSession, simpleBody,
                                             simpleHeaders, simpleStatus)
import           Servant.API                ((:<|>) (..), (:>), AuthProtect, BasicAuth, Capture, Delete,
                                             Get, Header (..),
                                             Headers, HttpVersion,
                                             IsSecure (..), JSON,
                                             NoContent (..), Patch, PlainText,
                                             Post, Put,
                                             QueryFlag, QueryParam, QueryParams,
                                             Raw, RemoteHost, ReqBody,
                                             StdMethod (..), Verb, addHeader)
import           Servant.API.Internal.Test.ComprehensiveAPI
import           Servant.Server             (ServantErr (..), Server, err401, err404,
                                             serve, Config((:.), EmptyConfig))
import           Test.Hspec                 (Spec, context, describe, it,
                                             shouldBe, shouldContain)
import qualified Test.Hspec.Wai as THW
import           Test.Hspec.Wai             (get, liftIO, matchHeaders,
                                             matchStatus, request,
                                             shouldRespondWith, with, (<:>))

import           Servant.API.Auth (BasicAuthData(BasicAuthData))
import           Servant.Server.Internal.Auth
                                            (AuthHandler, AuthReturnType, BasicAuthCheck (BasicAuthCheck),
                                               BasicAuthResult (Authorized, Unauthorized), mkAuthHandler)
import           Servant.Server.Internal.RoutingApplication
                                            (toApplication, RouteResult(..))
import           Servant.Server.Internal.Router
                                            (tweakResponse, runRouter,
                                             Router, Router'(LeafRouter))
import           Servant.Server.Internal.Config
                                            (NamedConfig(NamedConfig))

-- * comprehensive api test

-- This declaration simply checks that all instances are in place.
_ = serve comprehensiveAPI comprehensiveApiConfig

comprehensiveApiConfig :: Config '[NamedConfig "foo" '[]]
comprehensiveApiConfig = NamedConfig EmptyConfig :. EmptyConfig

-- * Specs

spec :: Spec
spec = do
  verbSpec
  captureSpec
  queryParamSpec
  reqBodySpec
  headerSpec
  rawSpec
  alternativeSpec
  responseHeadersSpec
  routerSpec
  miscCombinatorSpec
  authSpec

------------------------------------------------------------------------------
-- * verbSpec {{{
------------------------------------------------------------------------------

type VerbApi method status
    =                Verb method status '[JSON] Person
 :<|> "noContent" :> Verb method status '[JSON] NoContent
 :<|> "header"    :> Verb method status '[JSON] (Headers '[Header "H" Int] Person)
 :<|> "headerNC"  :> Verb method status '[JSON] (Headers '[Header "H" Int] NoContent)

verbSpec :: Spec
verbSpec = describe "Servant.API.Verb" $ do
  let server :: Server (VerbApi method status)
      server = return alice
          :<|> return NoContent
          :<|> return (addHeader 5 alice)
          :<|> return (addHeader 10 NoContent)
      get200     = Proxy :: Proxy (VerbApi 'GET 200)
      post210    = Proxy :: Proxy (VerbApi 'POST 210)
      put203     = Proxy :: Proxy (VerbApi 'PUT 203)
      delete280  = Proxy :: Proxy (VerbApi 'DELETE 280)
      patch214   = Proxy :: Proxy (VerbApi 'PATCH 214)
      wrongMethod m = if m == methodPatch then methodPost else methodPatch
      test desc api method (status :: Int) = context desc $

        with (return $ serve api EmptyConfig server) $ do

          -- HEAD and 214/215 need not return bodies
          unless (status `elem` [214, 215] || method == methodHead) $
            it "returns the person" $ do
              response <- Test.Hspec.Wai.request method "/" [] ""
              liftIO $ statusCode (simpleStatus response) `shouldBe` status
              liftIO $ decode' (simpleBody response) `shouldBe` Just alice

          it "returns no content on NoContent" $ do
              response <- Test.Hspec.Wai.request method "/noContent" [] ""
              liftIO $ statusCode (simpleStatus response) `shouldBe` status
              liftIO $ simpleBody response `shouldBe` ""

          -- HEAD should not return body
          when (method == methodHead) $
            it "HEAD returns no content body" $ do
              response <- Test.Hspec.Wai.request method "/" [] ""
              liftIO $ simpleBody response `shouldBe` ""

          it "throws 405 on wrong method " $ do
            Test.Hspec.Wai.request (wrongMethod method) "/" [] ""
              `shouldRespondWith` 405

          it "returns headers" $ do
            response1 <- Test.Hspec.Wai.request method "/header" [] ""
            liftIO $ statusCode (simpleStatus response1) `shouldBe` status
            liftIO $ simpleHeaders response1 `shouldContain` [("H", "5")]

            response2 <- Test.Hspec.Wai.request method "/header" [] ""
            liftIO $ statusCode (simpleStatus response2) `shouldBe` status
            liftIO $ simpleHeaders response2 `shouldContain` [("H", "5")]

          it "handles trailing '/' gracefully" $ do
            response <- Test.Hspec.Wai.request method "/headerNC/" [] ""
            liftIO $ statusCode (simpleStatus response) `shouldBe` status

          it "returns 406 if the Accept header is not supported" $ do
            Test.Hspec.Wai.request method "" [(hAccept, "crazy/mime")] ""
              `shouldRespondWith` 406

          it "responds if the Accept header is supported" $ do
            response <- Test.Hspec.Wai.request method ""
               [(hAccept, "application/json")] ""
            liftIO $ statusCode (simpleStatus response) `shouldBe` status

          it "sets the Content-Type header" $ do
            response <- Test.Hspec.Wai.request method "" [] ""
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
captureServer :: Integer -> ExceptT ServantErr IO Animal
captureServer legs = case legs of
  4 -> return jerry
  2 -> return tweety
  _ -> throwE err404

captureSpec :: Spec
captureSpec = do
  describe "Servant.API.Capture" $ do
    with (return (serve captureApi EmptyConfig captureServer)) $ do

      it "can capture parts of the 'pathInfo'" $ do
        response <- get "/2"
        liftIO $ decode' (simpleBody response) `shouldBe` Just tweety

      it "returns 404 if the decoding fails" $ do
        get "/notAnInt" `shouldRespondWith` 404

    with (return (serve
        (Proxy :: Proxy (Capture "captured" String :> Raw))
        EmptyConfig
        (\ "captured" request_ respond ->
            respond $ responseLBS ok200 [] (cs $ show $ pathInfo request_)))) $ do
      it "strips the captured path snippet from pathInfo" $ do
        get "/captured/foo" `shouldRespondWith` (fromString (show ["foo" :: String]))

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
        (flip runSession) (serve queryParamApi EmptyConfig qpServer) $ do
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
        (flip runSession) (serve queryParamApi EmptyConfig qpServer) $ do
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
        (flip runSession) (serve queryParamApi EmptyConfig qpServer) $ do
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
      mkReq method x = Test.Hspec.Wai.request method x
         [(hContentType, "application/json;charset=utf-8")]

  with (return $ serve reqBodyApi EmptyConfig server) $ do

    it "passes the argument to the handler" $ do
      response <- mkReq methodPost "" (encode alice)
      liftIO $ decode' (simpleBody response) `shouldBe` Just alice

    it "rejects invalid request bodies with status 400" $ do
      mkReq methodPut "/blah" "some invalid body" `shouldRespondWith` 400

    it "responds with 415 if the request body media type is unsupported" $ do
      Test.Hspec.Wai.request methodPost "/"
        [(hContentType, "application/nonsense")] "" `shouldRespondWith` 415

-- }}}
------------------------------------------------------------------------------
-- * headerSpec {{{
------------------------------------------------------------------------------

type HeaderApi a = Header "MyHeader" a :> Delete '[JSON] ()
headerApi :: Proxy (HeaderApi a)
headerApi = Proxy

headerSpec :: Spec
headerSpec = describe "Servant.API.Header" $ do

    let expectsInt :: Maybe Int -> ExceptT ServantErr IO ()
        expectsInt (Just x) = when (x /= 5) $ error "Expected 5"
        expectsInt Nothing  = error "Expected an int"

    let expectsString :: Maybe String -> ExceptT ServantErr IO ()
        expectsString (Just x) = when (x /= "more from you") $ error "Expected more from you"
        expectsString Nothing  = error "Expected a string"

    with (return (serve headerApi EmptyConfig expectsInt)) $ do
        let delete' x = Test.Hspec.Wai.request methodDelete x [("MyHeader", "5")]

        it "passes the header to the handler (Int)" $
            delete' "/" "" `shouldRespondWith` 200

    with (return (serve headerApi EmptyConfig expectsString)) $ do
        let delete' x = Test.Hspec.Wai.request methodDelete x [("MyHeader", "more from you")]

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
      (flip runSession) (serve rawApi EmptyConfig (rawApplication (const (42 :: Integer)))) $ do
        response <- Network.Wai.Test.request defaultRequest{
          pathInfo = ["foo"]
         }
        liftIO $ do
          simpleBody response `shouldBe` "42"

    it "gets the pathInfo modified" $ do
      (flip runSession) (serve rawApi EmptyConfig (rawApplication pathInfo)) $ do
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
  :<|> "bar" :> Delete '[JSON] ()

alternativeApi :: Proxy AlternativeApi
alternativeApi = Proxy

alternativeServer :: Server AlternativeApi
alternativeServer =
       return alice
  :<|> return jerry
  :<|> return "a string"
  :<|> return jerry
  :<|> return jerry
  :<|> return ()

alternativeSpec :: Spec
alternativeSpec = do
  describe "Servant.API.Alternative" $ do
    with (return $ serve alternativeApi EmptyConfig alternativeServer) $ do

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
  with (return $ serve (Proxy :: Proxy ResponseHeadersApi) EmptyConfig responseHeadersServer) $ do

    let methods = [methodGet, methodPost, methodPut, methodPatch]

    it "includes the headers in the response" $
      forM_ methods $ \method ->
        Test.Hspec.Wai.request method "/" [] ""
          `shouldRespondWith` "\"hi\""{ matchHeaders = ["H1" <:> "5", "H2" <:> "kilroy"]
                                      , matchStatus  = 200
                                      }

    it "responds with not found for non-existent endpoints" $
      forM_ methods $ \method ->
        Test.Hspec.Wai.request method "blahblah" [] ""
          `shouldRespondWith` 404

    it "returns 406 if the Accept header is not supported" $
      forM_ methods $ \method ->
        Test.Hspec.Wai.request method "" [(hAccept, "crazy/mime")] ""
          `shouldRespondWith` 406

-- }}}
------------------------------------------------------------------------------
-- * routerSpec {{{
------------------------------------------------------------------------------
routerSpec :: Spec
routerSpec = do
  describe "Servant.Server.Internal.Router" $ do
    let app' :: Application
        app' = toApplication $ runRouter router'

        router', router :: Router
        router' = tweakResponse (twk <$>) router
        router = LeafRouter $ \_ cont -> cont (Route $ responseBuilder (Status 201 "") [] "")

        twk :: Response -> Response
        twk (ResponseBuilder (Status i s) hs b) = ResponseBuilder (Status (i + 1) s) hs b
        twk b = b

    describe "tweakResponse" . with (return app') $ do
      it "calls f on route result" $ do
        get "" `shouldRespondWith` 202

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
miscCombinatorSpec = with (return $ serve miscApi EmptyConfig miscServ) $
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
-- * Authentication {{{
------------------------------------------------------------------------------
type AuthAPI = BasicAuth "foo" :> "basic" :> Get '[JSON] Animal
          :<|> AuthProtect "auth" :> "auth" :> Get '[JSON] Animal
authApi :: Proxy AuthAPI
authApi = Proxy
authServer :: Server AuthAPI
authServer = const (return jerry) :<|> const (return tweety)

type instance AuthReturnType (BasicAuth "foo") = ()
type instance AuthReturnType (AuthProtect "auth") = ()

authConfig :: Config '[ BasicAuthCheck ()
                      , AuthHandler Request ()
                      ]
authConfig =
  let basicHandler = BasicAuthCheck $ (\(BasicAuthData usr pass) ->
        if usr == "servant" && pass == "server"
        then return (Authorized ())
        else return Unauthorized
        )
      authHandler = (\req ->
        if elem ("Auth", "secret") (requestHeaders req)
        then return ()
        else throwE err401
        )
  in basicHandler :. mkAuthHandler authHandler :. EmptyConfig

authSpec :: Spec
authSpec = do
  describe "Servant.API.Auth" $ do
    with (return (serve authApi authConfig authServer)) $ do

      context "Basic Authentication" $ do
        it "returns with 401 with bad password" $ do
          get "/basic" `shouldRespondWith` 401
        it "returns 200 with the right password" $ do
          THW.request methodGet "/basic" [("Authorization","Basic c2VydmFudDpzZXJ2ZXI=")] "" `shouldRespondWith` 200

      context "Custom Auth Protection" $ do
        it "returns 401 when missing headers" $ do
          get "/auth" `shouldRespondWith` 401
        it "returns 200 with the right header" $ do
          THW.request methodGet "/auth" [("Auth","secret")] "" `shouldRespondWith` 200
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
-- }}}
