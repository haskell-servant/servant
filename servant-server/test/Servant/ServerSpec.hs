{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Servant.ServerSpec where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative        ((<$>))
#endif
import           Control.Monad              (forM_, when)
import           Control.Monad.Trans.Except (ExceptT, throwE)
import           Data.Aeson                 (FromJSON, ToJSON, decode', encode)
import           Data.ByteString            (ByteString)
import           Data.ByteString.Conversion ()
import           Data.Char                  (toUpper)
#if !MIN_VERSION_base(4,8,0)
import           Data.Monoid                ((<>), mempty)
#else
import           Data.Monoid                ((<>))
#endif
import           Data.Proxy                 (Proxy (Proxy))
import           Data.String                (fromString)
import           Data.String.Conversions    (cs)
import qualified Data.Text                  as T
import           GHC.Generics               (Generic)
import           Network.HTTP.Types         (hAccept, hContentType,
                                             methodDelete, methodGet, methodHead,
                                             methodPatch, methodPost, methodPut,
                                             ok200, parseQuery, ResponseHeaders, Status(..))
import           Network.Wai                (Application, Request, pathInfo,
                                             queryString, rawQueryString,
                                             responseLBS, responseBuilder)
import           Network.Wai.Internal       (Response(ResponseBuilder))
import           Network.Wai.Test           (defaultRequest, request,
                                             runSession, simpleBody, simpleHeaders, SResponse)
import           Servant.API                ((:<|>) (..), (:>), Capture, Delete,
                                             Get, Header (..), Headers,
                                             HttpVersion, IsSecure (..), JSON,
                                             Patch, PlainText, Post, Put,
                                             QueryFlag, QueryParam, QueryParams,
                                             Raw, RemoteHost, ReqBody,
                                             addHeader)
import           Servant.Server             (Server, serve, ServantErr(..), err404)
import           Test.Hspec                 (Spec, describe, it, shouldBe, shouldContain)
import           Test.Hspec.Wai             (get, liftIO, matchHeaders,
                                             matchStatus, post, request,
                                             shouldRespondWith, with, (<:>))
import           Test.Hspec.Wai.Internal     (WaiSession)
import           Servant.Server.Internal.RoutingApplication (toApplication)
import           Servant.Server.Internal.Router
                                            (tweakResponse, runRouter,
                                             Router, Router'(LeafRouter))
import           Servant.API.Authentication
import           Servant.Server.Internal.Authentication
import           Servant.Server.Internal.RoutingApplication (RouteResult(Route))
import           Web.JWT                    hiding (JSON)


-- * test data types

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


-- * specs

spec :: Spec
spec = do
  captureSpec
  getSpec
  headSpec
  postSpec
  putSpec
  patchSpec
  queryParamSpec
  headerSpec
  rawSpec
  unionSpec
  routerSpec
  responseHeadersSpec
  miscReqCombinatorsSpec
  basicAuthRequiredSpec
  jwtAuthRequiredSpec


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
    with (return (serve captureApi captureServer)) $ do

      it "can capture parts of the 'pathInfo'" $ do
        response <- get "/2"
        liftIO $ decode' (simpleBody response) `shouldBe` Just tweety

      it "returns 404 if the decoding fails" $ do
        get "/notAnInt" `shouldRespondWith` 404

    with (return (serve
        (Proxy :: Proxy (Capture "captured" String :> Raw))
        (\ "captured" request_ respond ->
            respond $ responseLBS ok200 [] (cs $ show $ pathInfo request_)))) $ do
      it "strips the captured path snippet from pathInfo" $ do
        get "/captured/foo" `shouldRespondWith` (fromString (show ["foo" :: String]))


type GetApi = Get '[JSON] Person
        :<|> "empty" :> Get '[] ()
        :<|> "post" :> Post '[] ()
getApi :: Proxy GetApi
getApi = Proxy

getSpec :: Spec
getSpec = do
  describe "Servant.API.Get" $ do
    let server = return alice :<|> return () :<|> return ()
    with (return $ serve getApi server) $ do

      it "allows to GET a Person" $ do
        response <- get "/"
        return response `shouldRespondWith` 200
        liftIO $ decode' (simpleBody response) `shouldBe` Just alice

      it "throws 405 (wrong method) on POSTs" $ do
        post "/" "" `shouldRespondWith` 405
        post "/empty" "" `shouldRespondWith` 405

      it "returns 204 if the type is '()'" $ do
        get "/empty" `shouldRespondWith` ""{ matchStatus = 204 }

      it "returns 406 if the Accept header is not supported" $ do
        Test.Hspec.Wai.request methodGet "" [(hAccept, "crazy/mime")] ""
          `shouldRespondWith` 406


headSpec :: Spec
headSpec = do
  describe "Servant.API.Head" $ do
    let server = return alice :<|> return () :<|> return ()
    with (return $ serve getApi server) $ do

      it "allows to GET a Person" $ do
        response <- Test.Hspec.Wai.request methodHead "/" [] ""
        return response `shouldRespondWith` 200
        liftIO $ decode' (simpleBody response) `shouldBe` (Nothing :: Maybe Person)

      it "does not allow HEAD to POST route" $ do
        response <- Test.Hspec.Wai.request methodHead "/post" [] ""
        return response `shouldRespondWith` 405

      it "throws 405 (wrong method) on POSTs" $ do
        post "/" "" `shouldRespondWith` 405
        post "/empty" "" `shouldRespondWith` 405

      it "returns 204 if the type is '()'" $ do
        response <- Test.Hspec.Wai.request methodHead "/empty" [] ""
        return response `shouldRespondWith` ""{ matchStatus = 204 }

      it "returns 406 if the Accept header is not supported" $ do
        Test.Hspec.Wai.request methodHead "" [(hAccept, "crazy/mime")] ""
          `shouldRespondWith` 406


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
      it "allows to retrieve simple GET parameters" $
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

      it "allows to retrieve lists in GET parameters" $
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


      it "allows to retrieve value-less GET parameters" $
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

type PostApi =
       ReqBody '[JSON] Person :> Post '[JSON] Integer
  :<|> "bla" :> ReqBody '[JSON] Person :> Post '[JSON] Integer
  :<|> "empty" :> Post '[] ()

postApi :: Proxy PostApi
postApi = Proxy

postSpec :: Spec
postSpec = do
  describe "Servant.API.Post and .ReqBody" $ do
    let server = return . age :<|> return . age :<|> return ()
    with (return $ serve postApi server) $ do
      let post' x = Test.Hspec.Wai.request methodPost x [(hContentType
                                                        , "application/json;charset=utf-8")]

      it "allows to POST a Person" $ do
        post' "/" (encode alice) `shouldRespondWith` "42"{
          matchStatus = 201
         }

      it "allows alternative routes if all have request bodies" $ do
        post' "/bla" (encode alice) `shouldRespondWith` "42"{
          matchStatus = 201
         }

      it "handles trailing '/' gracefully" $ do
        post' "/bla/" (encode alice) `shouldRespondWith` "42"{
          matchStatus = 201
         }

      it "correctly rejects invalid request bodies with status 400" $ do
        post' "/" "some invalid body" `shouldRespondWith` 400

      it "returns 204 if the type is '()'" $ do
        post' "empty" "" `shouldRespondWith` ""{ matchStatus = 204 }

      it "responds with 415 if the request body media type is unsupported" $ do
        let post'' x = Test.Hspec.Wai.request methodPost x [(hContentType
                                                            , "application/nonsense")]
        post'' "/" "anything at all" `shouldRespondWith` 415

type PutApi =
       ReqBody '[JSON] Person :> Put '[JSON] Integer
  :<|> "bla" :> ReqBody '[JSON] Person :> Put '[JSON] Integer
  :<|> "empty" :> Put '[] ()

putApi :: Proxy PutApi
putApi = Proxy

putSpec :: Spec
putSpec = do
  describe "Servant.API.Put and .ReqBody" $ do
    let server = return . age :<|> return . age :<|> return ()
    with (return $ serve putApi server) $ do
      let put' x = Test.Hspec.Wai.request methodPut x [(hContentType
                                                        , "application/json;charset=utf-8")]

      it "allows to put a Person" $ do
        put' "/" (encode alice) `shouldRespondWith` "42"{
          matchStatus = 200
         }

      it "allows alternative routes if all have request bodies" $ do
        put' "/bla" (encode alice) `shouldRespondWith` "42"{
          matchStatus = 200
         }

      it "handles trailing '/' gracefully" $ do
        put' "/bla/" (encode alice) `shouldRespondWith` "42"{
          matchStatus = 200
         }

      it "correctly rejects invalid request bodies with status 400" $ do
        put' "/" "some invalid body" `shouldRespondWith` 400

      it "returns 204 if the type is '()'" $ do
        put' "empty" "" `shouldRespondWith` ""{ matchStatus = 204 }

      it "responds with 415 if the request body media type is unsupported" $ do
        let put'' x = Test.Hspec.Wai.request methodPut x [(hContentType
                                                            , "application/nonsense")]
        put'' "/" "anything at all" `shouldRespondWith` 415

type PatchApi =
       ReqBody '[JSON] Person :> Patch '[JSON] Integer
  :<|> "bla" :> ReqBody '[JSON] Person :> Patch '[JSON] Integer
  :<|> "empty" :> Patch '[] ()

patchApi :: Proxy PatchApi
patchApi = Proxy

patchSpec :: Spec
patchSpec = do
  describe "Servant.API.Patch and .ReqBody" $ do
    let server = return . age :<|> return . age :<|> return ()
    with (return $ serve patchApi server) $ do
      let patch' x = Test.Hspec.Wai.request methodPatch x [(hContentType
                                                        , "application/json;charset=utf-8")]

      it "allows to patch a Person" $ do
        patch' "/" (encode alice) `shouldRespondWith` "42"{
          matchStatus = 200
         }

      it "allows alternative routes if all have request bodies" $ do
        patch' "/bla" (encode alice) `shouldRespondWith` "42"{
          matchStatus = 200
         }

      it "handles trailing '/' gracefully" $ do
        patch' "/bla/" (encode alice) `shouldRespondWith` "42"{
          matchStatus = 200
         }

      it "correctly rejects invalid request bodies with status 400" $ do
        patch' "/" "some invalid body" `shouldRespondWith` 400

      it "returns 204 if the type is '()'" $ do
        patch' "empty" "" `shouldRespondWith` ""{ matchStatus = 204 }

      it "responds with 415 if the request body media type is unsupported" $ do
        let patch'' x = Test.Hspec.Wai.request methodPatch x [(hContentType
                                                            , "application/nonsense")]
        patch'' "/" "anything at all" `shouldRespondWith` 415

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

    with (return (serve headerApi expectsInt)) $ do
        let delete' x = Test.Hspec.Wai.request methodDelete x [("MyHeader" ,"5")]

        it "passes the header to the handler (Int)" $
            delete' "/" "" `shouldRespondWith` 204

    with (return (serve headerApi expectsString)) $ do
        let delete' x = Test.Hspec.Wai.request methodDelete x [("MyHeader" ,"more from you")]

        it "passes the header to the handler (String)" $
            delete' "/" "" `shouldRespondWith` 204


type RawApi = "foo" :> Raw
rawApi :: Proxy RawApi
rawApi = Proxy
rawApplication :: Show a => (Request -> a) -> Application
rawApplication f request_ respond = respond $ responseLBS ok200 [] (cs $ show $ f request_)

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


type AlternativeApi =
       "foo" :> Get '[JSON] Person
  :<|> "bar" :> Get '[JSON] Animal
  :<|> "foo" :> Get '[PlainText] T.Text
  :<|> "bar" :> Post '[JSON] Animal
  :<|> "bar" :> Put '[JSON] Animal
  :<|> "bar" :> Delete '[JSON] ()
unionApi :: Proxy AlternativeApi
unionApi = Proxy

unionServer :: Server AlternativeApi
unionServer =
       return alice
  :<|> return jerry
  :<|> return "a string"
  :<|> return jerry
  :<|> return jerry
  :<|> return ()

unionSpec :: Spec
unionSpec = do
  describe "Servant.API.Alternative" $ do
    with (return $ serve unionApi unionServer) $ do

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

    let methods = [(methodGet, 200), (methodPost, 201), (methodPut, 200), (methodPatch, 200)]

    it "includes the headers in the response" $
      forM_ methods $ \(method, expected) ->
        Test.Hspec.Wai.request method "/" [] ""
          `shouldRespondWith` "\"hi\""{ matchHeaders = ["H1" <:> "5", "H2" <:> "kilroy"]
                                      , matchStatus  = expected
                                      }

    it "responds with not found for non-existent endpoints" $
      forM_ methods $ \(method,_) ->
        Test.Hspec.Wai.request method "blahblah" [] ""
          `shouldRespondWith` 404

    it "returns 406 if the Accept header is not supported" $
      forM_ methods $ \(method,_) ->
        Test.Hspec.Wai.request method "" [(hAccept, "crazy/mime")] ""
          `shouldRespondWith` 406


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

type PrioErrorsApi = ReqBody '[JSON] Person :> "foo" :> Get '[JSON] Integer

prioErrorsApi :: Proxy PrioErrorsApi
prioErrorsApi = Proxy

-- | Test the relative priority of error responses from the server.
--
-- In particular, we check whether matching continues even if a 'ReqBody'
-- or similar construct is encountered early in a path. We don't want to
-- see a complaint about the request body unless the path actually matches.
--
prioErrorsSpec :: Spec
prioErrorsSpec = describe "PrioErrors" $ do
  let server = return . age
  with (return $ serve prioErrorsApi server) $ do
    let check (mdescr, method) path (cdescr, ctype, body) resp =
          it fulldescr $
            Test.Hspec.Wai.request method path [(hContentType, ctype)] body
              `shouldRespondWith` resp
          where
            fulldescr = "returns " ++ show (matchStatus resp) ++ " on " ++ mdescr
                     ++ " " ++ cs path ++ " (" ++ cdescr ++ ")"

        get' = ("GET", methodGet)
        put' = ("PUT", methodPut)

        txt   = ("text"        , "text/plain;charset=utf8"      , "42"        )
        ijson = ("invalid json", "application/json;charset=utf8", "invalid"   )
        vjson = ("valid json"  , "application/json;charset=utf8", encode alice)

    check get' "/"    txt   404
    check get' "/bar" txt   404
    check get' "/foo" txt   415
    check put' "/"    txt   404
    check put' "/bar" txt   404
    check put' "/foo" txt   405
    check get' "/"    ijson 404
    check get' "/bar" ijson 404
    check get' "/foo" ijson 400
    check put' "/"    ijson 404
    check put' "/bar" ijson 404
    check put' "/foo" ijson 405
    check get' "/"    vjson 404
    check get' "/bar" vjson 404
    check get' "/foo" vjson 200
    check put' "/"    vjson 404
    check put' "/bar" vjson 404
    check put' "/foo" vjson 405

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

miscReqCombinatorsSpec :: Spec
miscReqCombinatorsSpec = with (return $ serve miscApi miscServ) $
  describe "Misc. combinators for request inspection" $ do
    it "Successfully gets the HTTP version specified in the request" $
      go "/version" "\"HTTP/1.0\""

    it "Checks that hspec-wai uses HTTP, not HTTPS" $
      go "/secure" "\"not secure\""

    it "Checks that hspec-wai issues request from 0.0.0.0" $
      go "/host" "\"0.0.0.0:0\""

  where go path res = Test.Hspec.Wai.get path `shouldRespondWith` res


-- | we include two endpoints /foo and /bar and we put the BasicAuth
-- portion in two different places
type AuthUser = ByteString
type BasicAuthFooRealm = AuthProtect (BasicAuth "foo-realm") AuthUser 'Strict
type BasicAuthBarRealm = AuthProtect (BasicAuth "bar-realm") AuthUser 'Strict
type BasicAuthRequiredAPI = BasicAuthFooRealm :> "foo" :> Get '[JSON] Person
                  :<|> "bar" :> BasicAuthBarRealm :> Get '[JSON] Animal

basicAuthFooCheck :: BasicAuth "foo-realm" -> IO (Maybe AuthUser)
basicAuthFooCheck (BasicAuth user pass) = if user == "servant" && pass == "server"
                                          then return (Just "servant")
                                          else return Nothing

basicAuthBarCheck :: BasicAuth "bar-realm" -> IO (Maybe AuthUser)
basicAuthBarCheck (BasicAuth usr pass) = if usr == "bar" && pass == "bar"
                                         then return (Just "bar")
                                         else return Nothing
basicBasicAuthRequiredApi :: Proxy BasicAuthRequiredAPI
basicBasicAuthRequiredApi = Proxy

basicAuthRequiredServer :: Server BasicAuthRequiredAPI
basicAuthRequiredServer = basicAuthStrict basicAuthFooCheck (const . return $ alice)
                :<|> basicAuthStrict basicAuthBarCheck (const . return $ jerry)

-- base64-encoded "servant:server"
base64ServantColonServer :: ByteString
base64ServantColonServer = "c2VydmFudDpzZXJ2ZXI="

-- base64-encoded "bar:bar"
base64BarColonPassword :: ByteString
base64BarColonPassword = "YmFyOmJhcg=="

-- base64-encoded "user:password"
base64UserColonPassword :: ByteString
base64UserColonPassword = "dXNlcjpwYXNzd29yZA=="

basicAuthGet :: ByteString -> ByteString -> WaiSession SResponse
basicAuthGet path base64EncodedAuth = Test.Hspec.Wai.request methodGet path [("Authorization", "Basic " <> base64EncodedAuth)] ""

basicAuthRequiredSpec :: Spec
basicAuthRequiredSpec = do
    describe "Servant.API.Authentication" $ do
        with (return $ serve basicBasicAuthRequiredApi basicAuthRequiredServer) $ do
            it "allows access with the correct username and password" $ do
                response1 <- basicAuthGet "/foo" base64ServantColonServer
                liftIO $ do
                    decode' (simpleBody response1) `shouldBe` Just alice

                response2 <- basicAuthGet "/bar" base64BarColonPassword
                liftIO $ do
                    decode' (simpleBody response2) `shouldBe` Just jerry

            it "rejects requests with the incorrect username and password" $ do
                basicAuthGet "/foo" base64UserColonPassword `shouldRespondWith` 401
                basicAuthGet "/bar" base64UserColonPassword `shouldRespondWith` 401

            it "does not respond to non-authenticated requests" $ do
                get "/foo" `shouldRespondWith` 401
                get "/bar" `shouldRespondWith` 401

            it "adds the appropriate header to rejected 401 requests" $ do
                foo401 <- get "/foo"
                bar401 <- get "/bar"
                liftIO $ do
                    let fooHeader = [("WWW-Authenticate", "Basic realm=\"foo-realm\"")] :: ResponseHeaders
                    let barHeader = [("WWW-Authenticate", "Basic realm=\"bar-realm\"")] :: ResponseHeaders
                    (simpleHeaders foo401) `shouldContain` fooHeader
                    (simpleHeaders bar401) `shouldContain` barHeader


type JWTAuthProtect = AuthProtect JWTAuth (JWT VerifiedJWT) 'Strict

type JWTAuthRequiredAPI = JWTAuthProtect :> "foo" :> Get '[JSON] Person


jwtAuthRequiredApi :: Proxy JWTAuthRequiredAPI
jwtAuthRequiredApi = Proxy

jwtAuthRequiredServer :: Server JWTAuthRequiredAPI
jwtAuthRequiredServer = jwtAuthStrict (secret "secret") (const . return $ alice)

correctToken = "blah"
incorrectToken = "blah"

jwtAuthGet :: ByteString -> ByteString -> WaiSession SResponse
jwtAuthGet path token = Test.Hspec.Wai.request methodGet path [("Authorization", "Bearer " <> token)] ""

jwtAuthRequiredSpec :: Spec
jwtAuthRequiredSpec = do
  describe "JWT Auth" $ do
    with (return $ serve jwtAuthRequiredApi jwtAuthRequiredServer) $ do
      it "allows access with the correct token" $ do
        response <- jwtAuthGet "/foo" correctToken
        liftIO $ do
          decode' (simpleBody response) `shouldBe` Just alice
      it "rejects requests with an incorrect token" $ do
        jwtAuthGet "/foo" incorrectToken `shouldRespondWith` 401 
      it "rejects requests without auth data" $ do
        get "/foo" `shouldRespondWith` 401
      it "responds correctly to requests without auth data" $ do
        a <- jwtAuthGet "/foo" incorrectToken
        let aHeader = [("WWW-Authenticate", "Bearer error=\"invalid_token\"")] :: ResponseHeaders
        liftIO (simpleHeaders a `shouldContain` aHeader)
      it "respond correctly to requests with incorrect auth data" $ do
        a <- get "/foo"
        let aHeader = [("WWW-Authenticate", "Bearer error=\"invalid_token\"")] :: ResponseHeaders
        liftIO (simpleHeaders a `shouldContain` aHeader)
