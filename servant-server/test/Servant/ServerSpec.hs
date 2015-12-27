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
import           Data.ByteString.Conversion ()
import           Data.Char                  (toUpper)
import           Data.Proxy                 (Proxy (Proxy))
import           Data.String                (fromString)
import           Data.String.Conversions    (cs)
import qualified Data.Text                  as T
import           GHC.Generics               (Generic)
import           Network.HTTP.Types         (hAccept, hContentType,
                                             methodDelete, methodGet, methodHead,
                                             methodPatch, methodPost, methodPut,
                                             ok200, parseQuery, Status(..))
import           Network.Wai                (Application, Request, pathInfo,
                                             queryString, rawQueryString,
                                             responseLBS, responseBuilder)
import           Network.Wai.Internal       (Response(ResponseBuilder))
import           Network.Wai.Test           (defaultRequest, request,
                                             runSession, simpleBody)
import           Servant.API                ((:<|>) (..), (:>), Capture, Delete,
                                             Get, Header (..), Headers,
                                             HttpVersion, IsSecure (..), JSON,
                                             Patch, PlainText, Post, Put,
                                             QueryFlag, QueryParam, QueryParams,
                                             Raw, RemoteHost, ReqBody,
                                             addHeader)
import           Servant.Server             (Server, serve, ServantErr(..), err404)
import           Test.Hspec                 (Spec, describe, it, shouldBe)
import           Test.Hspec.Wai             (get, liftIO, matchHeaders,
                                             matchStatus, post, request,
                                             shouldRespondWith, with, (<:>))
import           Servant.Server.Internal.RoutingApplication (toApplication, RouteResult(..))
import           Servant.Server.Internal.Router
                                            (tweakResponse, runRouter,
                                             Router, Router'(LeafRouter))


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
        :<|> "empty" :> Get '[JSON] ()
        :<|> "emptyWithHeaders" :> Get '[JSON] (Headers '[Header "H" Int] ())
        :<|> "post" :> Post '[JSON] ()

getApi :: Proxy GetApi
getApi = Proxy

getSpec :: Spec
getSpec = do
  describe "Servant.API.Get" $ do
    let server = return alice
            :<|> return ()
            :<|> return (addHeader 5 ())
            :<|> return ()

    with (return $ serve getApi server) $ do

      it "allows to GET a Person" $ do
        response <- get "/"
        return response `shouldRespondWith` 200
        liftIO $ decode' (simpleBody response) `shouldBe` Just alice

      it "throws 405 (wrong method) on POSTs" $ do
        post "/" "" `shouldRespondWith` 405
        post "/empty" "" `shouldRespondWith` 405

      it "returns headers" $ do
        get "/emptyWithHeaders" `shouldRespondWith` 200 { matchHeaders = [ "H" <:> "5" ] }

      it "returns 406 if the Accept header is not supported" $ do
        Test.Hspec.Wai.request methodGet "" [(hAccept, "crazy/mime")] ""
          `shouldRespondWith` 406


headSpec :: Spec
headSpec = do
  describe "Servant.API.Head" $ do
    let server = return alice
            :<|> return ()
            :<|> return (addHeader 5 ())
            :<|> return ()
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
  :<|> "empty" :> Post '[JSON] ()

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
          matchStatus = 200
         }

      it "allows alternative routes if all have request bodies" $ do
        post' "/bla" (encode alice) `shouldRespondWith` "42"{
          matchStatus = 200
         }

      it "handles trailing '/' gracefully" $ do
        post' "/bla/" (encode alice) `shouldRespondWith` "42"{
          matchStatus = 200
         }

      it "correctly rejects invalid request bodies with status 400" $ do
        post' "/" "some invalid body" `shouldRespondWith` 400

      it "responds with 415 if the request body media type is unsupported" $ do
        let post'' x = Test.Hspec.Wai.request methodPost x [(hContentType
                                                            , "application/nonsense")]
        post'' "/" "anything at all" `shouldRespondWith` 415

type PutApi =
       ReqBody '[JSON] Person :> Put '[JSON] Integer
  :<|> "bla" :> ReqBody '[JSON] Person :> Put '[JSON] Integer
  :<|> "empty" :> Put '[JSON] ()

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

      it "responds with 415 if the request body media type is unsupported" $ do
        let put'' x = Test.Hspec.Wai.request methodPut x [(hContentType
                                                            , "application/nonsense")]
        put'' "/" "anything at all" `shouldRespondWith` 415

type PatchApi =
       ReqBody '[JSON] Person :> Patch '[JSON] Integer
  :<|> "bla" :> ReqBody '[JSON] Person :> Patch '[JSON] Integer
  :<|> "empty" :> Patch '[JSON] ()

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
        let delete' x = Test.Hspec.Wai.request methodDelete x [("MyHeader", "5")]

        it "passes the header to the handler (Int)" $
            delete' "/" "" `shouldRespondWith` 200

    with (return (serve headerApi expectsString)) $ do
        let delete' x = Test.Hspec.Wai.request methodDelete x [("MyHeader", "more from you")]

        it "passes the header to the handler (String)" $
            delete' "/" "" `shouldRespondWith` 200


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

    let methods = [(methodGet, 200), (methodPost, 200), (methodPut, 200), (methodPatch, 200)]

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
