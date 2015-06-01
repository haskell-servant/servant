{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances       #-}

module Servant.ServerSpec where


import           Control.Monad              (forM_, when)
import           Control.Monad.Trans.Either (EitherT, left)
import           Data.Aeson                 (FromJSON, ToJSON, decode', encode)
import           Data.ByteString.Conversion ()
import           Data.Char                  (toUpper)
import           Data.Monoid                ((<>))
import           Data.Proxy                 (Proxy (Proxy))
import           Data.String                (fromString)
import           Data.String.Conversions    (cs)
import qualified Data.Text                  as T
import           GHC.Generics               (Generic)
import           Network.HTTP.Types         (hAccept, hContentType,
                                             methodDelete, methodGet,
                                             methodPatch, methodPost, methodPut,
                                             ok200, parseQuery, status409)
import           Network.Wai                (Application, Request, pathInfo,
                                             queryString, rawQueryString,
                                             responseLBS)
import           Network.Wai.Test           (defaultRequest, request,
                                             runSession, simpleBody)
import           Test.Hspec                 (Spec, describe, it, shouldBe)
import           Test.Hspec.Wai             (get, liftIO, matchHeaders,
                                             matchStatus, post, request,
                                             shouldRespondWith, with, (<:>))

import           Servant.API                ((:<|>) (..), (:>),
                                             addHeader, Capture,
                                             Delete, Get, Header (..), Headers,
                                             JSON, MatrixFlag, MatrixParam,
                                             MatrixParams, Patch, PlainText,
                                             Post, Put, QueryFlag, QueryParam,
                                             QueryParams, Raw, ReqBody)
import           Servant.Server             (Server, serve, ServantErr(..), err404)
import           Servant.Server.Internal    (RouteMismatch (..))


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
  postSpec
  putSpec
  patchSpec
  queryParamSpec
  matrixParamSpec
  headerSpec
  rawSpec
  unionSpec
  prioErrorsSpec
  errorsSpec
  responseHeadersSpec


type CaptureApi = Capture "legs" Integer :> Get '[JSON] Animal
captureApi :: Proxy CaptureApi
captureApi = Proxy
captureServer :: Integer -> EitherT ServantErr IO Animal
captureServer legs = case legs of
  4 -> return jerry
  2 -> return tweety
  _ -> left err404

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
getApi :: Proxy GetApi
getApi = Proxy

getSpec :: Spec
getSpec = do
  describe "Servant.API.Get" $ do
    let server = return alice :<|> return ()
    with (return $ serve getApi server) $ do

      it "allows to GET a Person" $ do
        response <- get "/"
        return response `shouldRespondWith` 200
        liftIO $ decode' (simpleBody response) `shouldBe` Just alice

      it "throws 405 (wrong method) on POSTs" $ do
        post "/" "" `shouldRespondWith` 405
        post "/empty" "" `shouldRespondWith` 405

      it "returns 204 if the type is '()'" $ do
        get "empty" `shouldRespondWith` ""{ matchStatus = 204 }

      it "returns 415 if the Accept header is not supported" $ do
        Test.Hspec.Wai.request methodGet "" [(hAccept, "crazy/mime")] ""
          `shouldRespondWith` 415



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
          response3' <- Network.Wai.Test.request defaultRequest{
            rawQueryString = params3'',
            queryString = parseQuery params3'',
            pathInfo = ["b"]
           }
          liftIO $
            decode' (simpleBody response3') `shouldBe` Just alice{
              name = "Alice"
             }

type MatrixParamApi = "a" :> MatrixParam "name" String :> Get '[JSON] Person
                :<|> "b" :> MatrixParams "names" String :> "bsub" :> MatrixParams "names" String :> Get '[JSON] Person
                :<|> "c" :> MatrixFlag "capitalize" :> Get '[JSON] Person
                :<|> "d" :> Capture "foo" Integer :> MatrixParam "name" String :> MatrixFlag "capitalize" :> "dsub" :> Get '[JSON] Person

matrixParamApi :: Proxy MatrixParamApi
matrixParamApi = Proxy

mpServer :: Server MatrixParamApi
mpServer = matrixParamServer :<|> mpNames :<|> mpCapitalize alice :<|> mpComplex
  where mpNames (_:name2:_) _ = return alice { name = name2 }
        mpNames _           _ = return alice

        mpCapitalize p False = return p
        mpCapitalize p True  = return p { name = map toUpper (name p) }

        matrixParamServer (Just name) = return alice{name = name}
        matrixParamServer Nothing = return alice

        mpAge age p = return p { age = age }
        mpComplex capture name cap = matrixParamServer name >>= flip mpCapitalize cap >>= mpAge capture

matrixParamSpec :: Spec
matrixParamSpec = do
  describe "Servant.API.MatrixParam" $ do
      it "allows to retrieve simple matrix parameters" $
        (flip runSession) (serve matrixParamApi mpServer) $ do
          response1 <- Network.Wai.Test.request defaultRequest{
            pathInfo = ["a;name=bob"]
           }
          liftIO $ do
            decode' (simpleBody response1) `shouldBe` Just alice{
              name = "bob"
             }

      it "allows to retrieve lists in matrix parameters" $
        (flip runSession) (serve matrixParamApi mpServer) $ do
          response2 <- Network.Wai.Test.request defaultRequest{
            pathInfo = ["b;names=bob;names=john", "bsub;names=anna;names=sarah"]
           }
          liftIO $
            decode' (simpleBody response2) `shouldBe` Just alice{
              name = "john"
             }

      it "allows to retrieve value-less matrix parameters" $
        (flip runSession) (serve matrixParamApi mpServer) $ do
          response3 <- Network.Wai.Test.request defaultRequest{
            pathInfo = ["c;capitalize"]
           }
          liftIO $
            decode' (simpleBody response3) `shouldBe` Just alice{
              name = "ALICE"
             }

          response3' <- Network.Wai.Test.request defaultRequest{
            pathInfo = ["c;capitalize="]
           }
          liftIO $
            decode' (simpleBody response3') `shouldBe` Just alice{
              name = "ALICE"
             }

      it "allows to retrieve matrix parameters on captured segments" $
        (flip runSession) (serve matrixParamApi mpServer) $ do
          response4 <- Network.Wai.Test.request defaultRequest{
            pathInfo = ["d", "12;name=stephen;capitalize", "dsub"]
           }
          liftIO $
            decode' (simpleBody response4) `shouldBe` Just alice{
              name = "STEPHEN",
              age = 12
             }

          response4' <- Network.Wai.Test.request defaultRequest{
            pathInfo = ["d;ignored=1", "5", "dsub"]
           }
          liftIO $
            decode' (simpleBody response4') `shouldBe` Just alice{
              name = "Alice",
              age = 5
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

      it "responds with 415 if the requested media type is unsupported" $ do
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

      it "responds with 415 if the requested media type is unsupported" $ do
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

      it "responds with 415 if the requested media type is unsupported" $ do
        let patch'' x = Test.Hspec.Wai.request methodPatch x [(hContentType
                                                            , "application/nonsense")]
        patch'' "/" "anything at all" `shouldRespondWith` 415

type HeaderApi a = Header "MyHeader" a :> Delete '[JSON] ()
headerApi :: Proxy (HeaderApi a)
headerApi = Proxy

headerSpec :: Spec
headerSpec = describe "Servant.API.Header" $ do

    let expectsInt :: Maybe Int -> EitherT ServantErr IO ()
        expectsInt (Just x) = when (x /= 5) $ error "Expected 5"
        expectsInt Nothing  = error "Expected an int"

    let expectsString :: Maybe String -> EitherT ServantErr IO ()
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

    it "returns 415 if the Accept header is not supported" $
      forM_ methods $ \(method,_) ->
        Test.Hspec.Wai.request method "" [(hAccept, "crazy/mime")] ""
          `shouldRespondWith` 415

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

-- | Test server error functionality.
errorsSpec :: Spec
errorsSpec = do
  let he = HttpError status409 (Just "A custom error")
  let ib = InvalidBody "The body is invalid"
  let wm = WrongMethod
  let nf = NotFound

  describe "Servant.Server.Internal.RouteMismatch" $ do
    it "HttpError > *" $ do
      ib <> he `shouldBe` he
      wm <> he `shouldBe` he
      nf <> he `shouldBe` he

      he <> ib `shouldBe` he
      he <> wm `shouldBe` he
      he <> nf `shouldBe` he

    it "HE > InvalidBody > (WM,NF)" $ do
      he <> ib `shouldBe` he
      wm <> ib `shouldBe` ib
      nf <> ib `shouldBe` ib

      ib <> he `shouldBe` he
      ib <> wm `shouldBe` ib
      ib <> nf `shouldBe` ib

    it "HE > IB > WrongMethod > NF" $ do
      he <> wm `shouldBe` he
      ib <> wm `shouldBe` ib
      nf <> wm `shouldBe` wm

      wm <> he `shouldBe` he
      wm <> ib `shouldBe` ib
      wm <> nf `shouldBe` wm

    it "* > NotFound" $ do
      he <> nf `shouldBe` he
      ib <> nf `shouldBe` ib
      wm <> nf `shouldBe` wm

      nf <> he `shouldBe` he
      nf <> ib `shouldBe` ib
      nf <> wm `shouldBe` wm
