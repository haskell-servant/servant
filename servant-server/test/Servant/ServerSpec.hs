{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -freduction-depth=100 #-}

module Servant.ServerSpec where

import Control.Monad (forM_, join, unless, when)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ask, runReaderT)
import Data.Acquire (Acquire, mkAcquire)
import Data.Aeson (FromJSON, ToJSON, decode', encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.String (fromString)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Types
  ( QueryItem
  , Status (..)
  , hAccept
  , hContentType
  , imATeapot418
  , methodDelete
  , methodGet
  , methodHead
  , methodPatch
  , methodPost
  , methodPut
  , ok200
  , parseQuery
  )
import Network.Wai
  ( Application
  , Middleware
  , Request
  , pathInfo
  , queryString
  , rawQueryString
  , requestHeaders
  , responseLBS
  )
import Network.Wai.Test
  ( defaultRequest
  , request
  , runSession
  , simpleBody
  , simpleHeaders
  , simpleStatus
  )
import Prelude.Compat
import Servant.API
  ( AuthProtect
  , BasicAuth
  , BasicAuthData (BasicAuthData)
  , Capture
  , Capture'
  , CaptureAll
  , DeepQuery
  , Delete
  , Description
  , EmptyAPI
  , Fragment
  , Get
  , HasStatus (StatusOf)
  , Header
  , Header'
  , Headers
  , HttpVersion
  , IsSecure (..)
  , JSON
  , Lenient
  , NoContent (..)
  , NoContentVerb
  , NoFraming
  , OctetStream
  , Optional
  , Patch
  , PlainText
  , Post
  , Put
  , QueryFlag
  , QueryParam
  , QueryParams
  , QueryString
  , Raw
  , RawM
  , RemoteHost
  , ReqBody
  , ReqBody'
  , SourceIO
  , StdMethod (..)
  , Stream
  , Strict
  , UVerb
  , Union
  , Verb
  , WithStatus (..)
  , addHeader
  , addHeader'
  , (:<|>) (..)
  , (:>)
  )
import Servant.API.QueryString (FromDeepQuery (..))
import Servant.API.Status (KnownStatus (..), statusFromNat)
import Servant.Test.ComprehensiveAPI
import qualified Servant.Types.SourceT as S
import Test.Hspec (Spec, context, describe, it, shouldBe, shouldContain)
import Test.Hspec.Wai
  ( get
  , matchHeaders
  , matchStatus
  , shouldRespondWith
  , with
  , (<:>)
  )
import qualified Test.Hspec.Wai as THW
import Text.Read (readMaybe)
import Prelude ()

import Servant.Server
  ( Context (EmptyContext, (:.))
  , Handler
  , Server
  , ServerT
  , Tagged (..)
  , emptyServer
  , err401
  , err403
  , err404
  , hoistServer
  , respond
  , serve
  , serveWithContext
  )
import Servant.Server.Experimental.Auth
  ( AuthHandler
  , AuthServerData
  , mkAuthHandler
  )
import Servant.Server.Internal.BasicAuth
  ( BasicAuthCheck (BasicAuthCheck)
  , BasicAuthResult (Authorized, Unauthorized)
  )
import Servant.Server.Internal.Context (NamedContext (..))

-- * KnownStatus instances for non-standard test status codes

instance KnownStatus 210 where statusVal _ = statusFromNat (Proxy @210)
instance KnownStatus 214 where statusVal _ = statusFromNat (Proxy @214)
instance KnownStatus 280 where statusVal _ = statusFromNat (Proxy @280)

-- * comprehensive api test

-- This declaration simply checks that all instances are in place.
_ = serveWithContext comprehensiveAPI comprehensiveApiContext

comprehensiveApiContext :: Context '[NamedContext "foo" '[], Acquire Int]
comprehensiveApiContext =
  NamedContext EmptyContext
    :. mkAcquire (pure 10) (\_ -> pure ())
    :. EmptyContext

-- * Specs

spec :: Spec
spec = do
  verbSpec
  uverbSpec
  captureSpec
  captureAllSpec
  queryParamSpec
  fragmentSpec
  reqBodySpec
  headerSpec
  rawSpec
  rawMSpec
  alternativeSpec
  responseHeadersSpec
  uverbResponseHeadersSpec
  miscCombinatorSpec
  basicAuthSpec
  genAuthSpec

------------------------------------------------------------------------------

-- * verbSpec {{{

------------------------------------------------------------------------------

type VerbApi method status =
  Verb method status '[JSON] Person
    :<|> "noContent" :> NoContentVerb method
    :<|> "header" :> Verb method status '[JSON] (Headers '[Header "H" Int] Person)
    :<|> "headerNC" :> Verb method status '[JSON] (Headers '[Header "H" Int] NoContent)
    :<|> "headerD" :> Verb method status '[JSON] (Headers '[Header' '[Description "desc", Optional, Strict] "H" Int] Person)
    :<|> "accept"
      :> ( Verb method status '[JSON] Person
             :<|> Verb method status '[PlainText] String
         )
    :<|> "stream" :> Stream method status NoFraming OctetStream (SourceIO BS.ByteString)

verbSpec :: Spec
verbSpec = describe "Servant.API.Verb" $ do
  let server :: Server (VerbApi method status)
      server =
        pure alice
          :<|> pure NoContent
          :<|> pure (addHeader 5 alice)
          :<|> pure (addHeader 10 NoContent)
          :<|> pure (addHeader' 5 alice)
          :<|> (pure alice :<|> pure "B")
          :<|> pure (S.source ["bytestring"])

      get200 = Proxy :: Proxy (VerbApi 'GET 200)
      post210 = Proxy :: Proxy (VerbApi 'POST 210)
      put203 = Proxy :: Proxy (VerbApi 'PUT 203)
      delete280 = Proxy :: Proxy (VerbApi 'DELETE 280)
      patch214 = Proxy :: Proxy (VerbApi 'PATCH 214)
      wrongMethod m = if m == methodPatch then methodPost else methodPatch
      test desc api method (status :: Int) = context desc $
        with (pure $ serve api server) $ do
          -- HEAD and 214/215 need not return bodies
          unless (status `elem` [214, 215] || method == methodHead) $
            it "returns the person" $ do
              response <- THW.request method "/" [] ""
              liftIO $ statusCode (simpleStatus response) `shouldBe` status
              liftIO $ decode' (simpleBody response) `shouldBe` Just alice

          it "returns no content on NoContent" $ do
            response <- THW.request method "/noContent" [] ""
            liftIO $ statusCode (simpleStatus response) `shouldBe` 204
            liftIO $ simpleBody response `shouldBe` ""

          -- HEAD should not return body
          when (method == methodHead) $
            it "HEAD returns no content body" $ do
              response <- THW.request method "/" [] ""
              liftIO $ simpleBody response `shouldBe` ""

          it "throws 405 on wrong method " $
            THW.request (wrongMethod method) "/" [] ""
              `shouldRespondWith` 405

          it "returns headers" $ do
            response1 <- THW.request method "/header" [] ""
            liftIO $ statusCode (simpleStatus response1) `shouldBe` status
            liftIO $ simpleHeaders response1 `shouldContain` [("H", "5")]

            response2 <- THW.request method "/header" [] ""
            liftIO $ statusCode (simpleStatus response2) `shouldBe` status
            liftIO $ simpleHeaders response2 `shouldContain` [("H", "5")]

            response3 <- THW.request method "/headerD" [] ""
            liftIO $ statusCode (simpleStatus response3) `shouldBe` status
            liftIO $ simpleHeaders response3 `shouldContain` [("H", "5")]

          it "handles trailing '/' gracefully" $ do
            response <- THW.request method "/headerNC/" [] ""
            liftIO $ statusCode (simpleStatus response) `shouldBe` status

          it "returns 406 if the Accept header is not supported" $
            THW.request method "" [(hAccept, "crazy/mime")] ""
              `shouldRespondWith` 406

          it "responds if the Accept header is supported" $ do
            response <-
              THW.request
                method
                ""
                [(hAccept, "application/json")]
                ""
            liftIO $ statusCode (simpleStatus response) `shouldBe` status

          unless (status `elem` [214, 215] || method == methodHead) $
            it "allows modular specification of supported content types" $ do
              response <- THW.request method "/accept" [(hAccept, "text/plain")] ""
              liftIO $ statusCode (simpleStatus response) `shouldBe` status
              liftIO $ simpleBody response `shouldBe` "B"

          it "sets the Content-Type header" $ do
            response <- THW.request method "" [] ""
            liftIO $
              simpleHeaders response
                `shouldContain` [("Content-Type", "application/json;charset=utf-8")]

          it "works for Stream as for Result" $ do
            response <- THW.request method "/stream" [] ""
            liftIO $ statusCode (simpleStatus response) `shouldBe` status
            liftIO $ simpleBody response `shouldBe` "bytestring"

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

type CaptureApi =
  Capture "legs" Integer :> Get '[JSON] Animal
    :<|> "ears" :> Capture' '[Lenient] "ears" Integer :> Get '[JSON] Animal
    :<|> "eyes" :> Capture' '[Strict] "eyes" Integer :> Get '[JSON] Animal

captureApi :: Proxy CaptureApi
captureApi = Proxy

captureServer :: Server CaptureApi
captureServer = getLegs :<|> getEars :<|> getEyes
  where
    getLegs :: Integer -> Handler Animal
    getLegs legs = case legs of
      4 -> pure jerry
      2 -> pure tweety
      _ -> throwError err404

    getEars :: Either String Integer -> Handler Animal
    getEars (Left _) = pure chimera -- ignore integer parse error, return weird animal
    getEars (Right 2) = pure jerry
    getEars (Right _) = throwError err404

    getEyes :: Integer -> Handler Animal
    getEyes 2 = pure jerry
    getEyes _ = throwError err404

captureSpec :: Spec
captureSpec = describe "Servant.API.Capture" $ do
  with (pure (serve captureApi captureServer)) $ do
    it "can capture parts of the 'pathInfo'" $ do
      response <- get "/2"
      liftIO $ decode' (simpleBody response) `shouldBe` Just tweety

    it "returns 400 if the decoding fails" $ get "/notAnInt" `shouldRespondWith` 400

    it "returns an animal if eyes or ears are 2" $ do
      get "/ears/2" `shouldRespondWith` 200
      get "/eyes/2" `shouldRespondWith` 200

    it "returns a weird animal on Lenient Capture" $ do
      response <- get "/ears/bla"
      liftIO $ decode' (simpleBody response) `shouldBe` Just chimera

    it "returns 400 if parsing integer fails on Strict Capture" $ get "/eyes/bla" `shouldRespondWith` 400

  with
    ( pure
        ( serve
            (Proxy :: Proxy (Capture "captured" String :> Raw))
            ( \"captured" -> Tagged $ \request_ sendResponse ->
                sendResponse $ responseLBS ok200 [] (BSL8.pack $ show $ pathInfo request_)
            )
        )
    )
    $ it "strips the captured path snippet from pathInfo"
    $ get "/captured/foo" `shouldRespondWith` fromString (show ["foo" :: String])

-- }}}
------------------------------------------------------------------------------

-- * captureAllSpec {{{

------------------------------------------------------------------------------

type CaptureAllApi =
  "legs" :> CaptureAll "legs" Integer :> Get '[JSON] Animal
    :<|> "arms" :> CaptureAll "arms" String :> Get '[JSON] [String]

captureAllApi :: Proxy CaptureAllApi
captureAllApi = Proxy

captureAllServer :: Server CaptureAllApi
captureAllServer = handleLegs :<|> pure
  where
    handleLegs [] = pure beholder
    handleLegs legs = case sum legs of
      4 -> pure jerry
      2 -> pure tweety
      _ -> throwError err404

type RootedCaptureAllApi = CaptureAll "xs" String :> Get '[JSON] [String]

captureAllSpec :: Spec
captureAllSpec = do
  let getStringList = decode' @[String] . simpleBody

  describe "Servant.API.CaptureAll" $ do
    with (pure (serve captureAllApi captureAllServer)) $ do
      it "can capture a single element of the 'pathInfo'" $ do
        response <- get "/legs/2"
        liftIO $ decode' (simpleBody response) `shouldBe` Just tweety

      it "can capture multiple elements of the 'pathInfo'" $ do
        response <- get "/legs/2/2"
        liftIO $ decode' (simpleBody response) `shouldBe` Just jerry

      it "can capture arbitrarily many elements of the 'pathInfo'" $ do
        response <- get "/legs/1/1/0/1/0/1"
        liftIO $ decode' (simpleBody response) `shouldBe` Just jerry

      it "can capture when there are no elements in 'pathInfo'" $ do
        response <- get "/legs/"
        liftIO $ decode' (simpleBody response) `shouldBe` Just beholder

      it "returns 400 if the decoding fails" $ get "/legs/notAnInt" `shouldRespondWith` 400

      it "returns 400 if the decoding fails, regardless of which element" $ get "/legs/1/0/0/notAnInt/3/" `shouldRespondWith` 400

      it "returns 400 if the decoding fails, even when it's multiple elements" $ get "/legs/1/0/0/notAnInt/3/orange/" `shouldRespondWith` 400

      it "can capture single String" $ do
        response <- get "/arms/jerry"
        liftIO $ getStringList response `shouldBe` Just ["jerry"]

      it "can capture when there are no elements in 'pathinfo'" $ do
        response <- get "/arms/"
        liftIO $ getStringList response `shouldBe` Just []

      it "can capture empty string from captureall" $ do
        response <- get "/arms//"
        liftIO $ getStringList response `shouldBe` Just [""]

    with (pure (serve (Proxy :: Proxy RootedCaptureAllApi) pure)) $ do
      it "can capture empty rooted capture all" $ do
        response <- get "/"
        liftIO $ getStringList response `shouldBe` Just []

      it "can capture empty string from rooted capture all" $ do
        response <- get "//"
        liftIO $ getStringList response `shouldBe` Just [""]

    with
      ( pure
          ( serve
              (Proxy :: Proxy (CaptureAll "segments" String :> Raw))
              ( \_captured -> Tagged $ \request_ sendResponse ->
                  sendResponse $ responseLBS ok200 [] (BSL8.pack $ show $ pathInfo request_)
              )
          )
      )
      $ it "consumes everything from pathInfo"
      $ get "/captured/foo/bar/baz" `shouldRespondWith` fromString (show ([] :: [Int]))

-- }}}
------------------------------------------------------------------------------

-- * queryParamSpec {{{

------------------------------------------------------------------------------

data Filter = Filter
  { ageFilter :: Integer
  , nameFilter :: String
  }
  deriving (Show)

instance FromDeepQuery Filter where
  fromDeepQuery params = do
    let maybeToRight l = maybe (Left l) Right
    age' <- maybeToRight "missing age" $ readMaybe . T.unpack =<< join (lookup ["age"] params)
    name' <- maybeToRight "missing name" $ join $ lookup ["name"] params
    pure $ Filter age' (T.unpack name')

type QueryParamApi =
  QueryParam "name" String :> Get '[JSON] Person
    :<|> "a" :> QueryParams "names" String :> Get '[JSON] Person
    :<|> "b" :> QueryFlag "capitalize" :> Get '[JSON] Person
    :<|> "param" :> QueryParam "age" Integer :> Get '[JSON] Person
    :<|> "multiparam" :> QueryParams "ages" Integer :> Get '[JSON] Person
    :<|> "raw-query-string" :> QueryString :> Get '[JSON] Person
    :<|> "deep-query" :> DeepQuery "filter" Filter :> Get '[JSON] Person

queryParamApi :: Proxy QueryParamApi
queryParamApi = Proxy

qpServer :: Server QueryParamApi
qpServer = queryParamServer :<|> qpNames :<|> qpCapitalize :<|> qpAge :<|> qpAges :<|> qpRaw :<|> qpDeep
  where
    qpNames (_ : name2 : _) = pure alice{name = name2}
    qpNames _ = pure alice

    qpCapitalize False = pure alice
    qpCapitalize True = pure alice{name = map toUpper (name alice)}

    qpAge Nothing = pure alice
    qpAge (Just age') = pure alice{age = age'}

    qpAges ages = pure alice{age = sum ages}

    qpRaw q =
      pure
        alice
          { name = maybe mempty BS8.unpack $ join (lookup "name" q)
          , age = fromMaybe 0 (readMaybe . BS8.unpack =<< join (lookup "age" q))
          }

    qpDeep filter' =
      pure
        alice
          { name = nameFilter filter'
          , age = ageFilter filter'
          }

    queryParamServer (Just name_) = pure alice{name = name_}
    queryParamServer Nothing = pure alice

queryParamSpec :: Spec
queryParamSpec = do
  let mkRequest params pinfo =
        Network.Wai.Test.request
          defaultRequest
            { rawQueryString = params
            , queryString = parseQuery params
            , pathInfo = pinfo
            }

  describe "Servant.API.QueryParam" $ do
    it "allows retrieving simple GET parameters" $
      flip runSession (serve queryParamApi qpServer) $ do
        response1 <- mkRequest "?name=bob" []
        liftIO $
          decode' (simpleBody response1)
            `shouldBe` Just
              alice
                { name = "bob"
                }

    it "allows retrieving lists in GET parameters" $
      flip runSession (serve queryParamApi qpServer) $ do
        response2 <- mkRequest "?names[]=bob&names[]=john" ["a"]
        liftIO $
          decode' (simpleBody response2)
            `shouldBe` Just
              alice
                { name = "john"
                }

    it "parses a query parameter" $
      flip runSession (serve queryParamApi qpServer) $ do
        response <- mkRequest "?age=55" ["param"]
        liftIO $
          decode' (simpleBody response)
            `shouldBe` Just
              alice
                { age = 55
                }

    it "generates an error on query parameter parse failure" $
      flip runSession (serve queryParamApi qpServer) $ do
        response <- mkRequest "?age=foo" ["param"]
        liftIO $ statusCode (simpleStatus response) `shouldBe` 400
        pure ()

    it "parses multiple query parameters" $
      flip runSession (serve queryParamApi qpServer) $ do
        response <- mkRequest "?ages=10&ages=22" ["multiparam"]
        liftIO $
          decode' (simpleBody response)
            `shouldBe` Just
              alice
                { age = 32
                }

    it "generates an error on parse failures of multiple parameters" $
      flip runSession (serve queryParamApi qpServer) $ do
        response <- mkRequest "?ages=2&ages=foo" ["multiparam"]
        liftIO $ statusCode (simpleStatus response) `shouldBe` 400
        pure ()

    it "allows retrieving value-less GET parameters" $
      flip runSession (serve queryParamApi qpServer) $ do
        response3 <- mkRequest "?capitalize" ["b"]
        liftIO $
          decode' (simpleBody response3)
            `shouldBe` Just
              alice
                { name = "ALICE"
                }

        response3' <- mkRequest "?capitalize=" ["b"]
        liftIO $
          decode' (simpleBody response3')
            `shouldBe` Just
              alice
                { name = "ALICE"
                }

        response3'' <- mkRequest "?unknown=" ["b"]
        liftIO $
          decode' (simpleBody response3'')
            `shouldBe` Just
              alice
                { name = "Alice"
                }

    it "allows retrieving a full query string" $
      flip runSession (serve queryParamApi qpServer) $ do
        response <- mkRequest "?age=32&name=john" ["raw-query-string"]
        liftIO $
          decode' (simpleBody response)
            `shouldBe` Just
              alice
                { name = "john"
                , age = 32
                }

    it "allows retrieving a query string deep object" $
      flip runSession (serve queryParamApi qpServer) $ do
        response <- mkRequest "?filter[age]=32&filter[name]=john" ["deep-query"]
        liftIO $
          decode' (simpleBody response)
            `shouldBe` Just
              alice
                { name = "john"
                , age = 32
                }

    describe "Uses queryString instead of rawQueryString" $ do
      -- test query parameters rewriter
      let queryRewriter :: Middleware
          queryRewriter app req =
            app
              req
                { queryString = rewrite <$> queryString req
                }
            where
              rewrite :: QueryItem -> QueryItem
              rewrite (k, v) = (fromMaybe k (BS.stripPrefix "person_" k), v)

      let app = queryRewriter $ serve queryParamApi qpServer

      it "allows rewriting for simple GET/query parameters" $
        flip runSession app $ do
          response1 <- mkRequest "?person_name=bob" []
          liftIO $
            decode' (simpleBody response1)
              `shouldBe` Just
                alice
                  { name = "bob"
                  }

      it "allows rewriting for lists in GET parameters" $
        flip runSession app $ do
          response2 <- mkRequest "?person_names[]=bob&person_names[]=john" ["a"]
          liftIO $
            decode' (simpleBody response2)
              `shouldBe` Just
                alice
                  { name = "john"
                  }

      it "allows rewriting when parsing multiple query parameters" $
        flip runSession app $ do
          response <- mkRequest "?person_ages=10&person_ages=22" ["multiparam"]
          liftIO $
            decode' (simpleBody response)
              `shouldBe` Just
                alice
                  { age = 32
                  }

      it "allows retrieving value-less GET parameters" $
        flip runSession app $ do
          response3 <- mkRequest "?person_capitalize" ["b"]
          liftIO $
            decode' (simpleBody response3)
              `shouldBe` Just
                alice
                  { name = "ALICE"
                  }

          response3' <- mkRequest "?person_capitalize=" ["b"]
          liftIO $
            decode' (simpleBody response3')
              `shouldBe` Just
                alice
                  { name = "ALICE"
                  }

          response3'' <- mkRequest "?person_unknown=" ["b"]
          liftIO $
            decode' (simpleBody response3'')
              `shouldBe` Just
                alice
                  { name = "Alice"
                  }

-- }}}
------------------------------------------------------------------------------

-- * fragmentSpec {{{

------------------------------------------------------------------------------

type FragmentApi =
  "name" :> Fragment String :> Get '[JSON] Person
    :<|> "age" :> Fragment Integer :> Get '[JSON] Person

fragmentApi :: Proxy FragmentApi
fragmentApi = Proxy

fragServer :: Server FragmentApi
fragServer = fragmentServer :<|> fragAge
  where
    fragmentServer = pure alice
    fragAge = pure alice

fragmentSpec :: Spec
fragmentSpec = do
  let mkRequest params pinfo =
        Network.Wai.Test.request
          defaultRequest
            { rawQueryString = params
            , queryString = parseQuery params
            , pathInfo = pinfo
            }

  describe "Servant.API.Fragment" $ it "ignores fragment even if it is present in query" $ flip runSession (serve fragmentApi fragServer) $ do
    response1 <- mkRequest "#Alice" ["name"]
    liftIO $ decode' (simpleBody response1) `shouldBe` Just alice

-- }}}
------------------------------------------------------------------------------

-- * reqBodySpec {{{

------------------------------------------------------------------------------
type ReqBodyApi =
  ReqBody '[JSON] Person :> Post '[JSON] Person
    :<|> "blah" :> ReqBody '[JSON] Person :> Put '[JSON] Integer
    :<|> "meh" :> ReqBody' '[Optional, Strict] '[JSON] Person :> Put '[JSON] Integer

reqBodyApi :: Proxy ReqBodyApi
reqBodyApi = Proxy

reqBodySpec :: Spec
reqBodySpec = describe "Servant.API.ReqBody" $ do
  let server :: Server ReqBodyApi
      server = pure :<|> pure . age :<|> pure . maybe 0 age
      mkReq method x =
        THW.request
          method
          x
          [(hContentType, "application/json;charset=utf-8")]

  with (pure $ serve reqBodyApi server) $ do
    it "passes the argument to the handler" $ do
      response <- mkReq methodPost "" (encode alice)
      liftIO $ decode' (simpleBody response) `shouldBe` Just alice

    it "rejects invalid request bodies with status 400" $ mkReq methodPut "/blah" "some invalid body" `shouldRespondWith` 400

    it "responds with 415 if the request body media type is unsupported" $
      THW.request
        methodPost
        "/"
        [(hContentType, "application/nonsense")]
        ""
        `shouldRespondWith` 415

    describe "optional request body" $ do
      it "request without body succeeds" $ do
        THW.request methodPut "/meh" [] mempty `shouldRespondWith` 200

      it "request without body responds with proper default value" $ do
        response <- THW.request methodPut "/meh" [] mempty
        liftIO $ simpleBody response `shouldBe` encode (0 :: Integer)

      it "responds with 415 if the request body media type is unsupported" $ do
        THW.request
          methodPut
          "/meh"
          [(hContentType, "application/nonsense")]
          (encode alice)
          `shouldRespondWith` 415
        THW.request
          methodPut
          "/meh"
          [(hContentType, "application/octet-stream")]
          (encode alice)
          `shouldRespondWith` 415

      it "request without body and with content-type header succeeds" $ do
        mkReq methodPut "/meh" mempty `shouldRespondWith` 200

      it "request without body and with content-type header returns default value" $ do
        response <- mkReq methodPut "/meh" mempty
        liftIO $ simpleBody response `shouldBe` encode (0 :: Integer)

      it "optional request body can be provided" $ do
        response <- mkReq methodPut "/meh" (encode alice)
        liftIO $ simpleBody response `shouldBe` encode (age alice)

-- }}}
------------------------------------------------------------------------------

-- * headerSpec {{{

------------------------------------------------------------------------------

type HeaderApi a = Header "MyHeader" a :> Delete '[JSON] NoContent

headerApi :: Proxy a -> Proxy (HeaderApi a)
headerApi _ = Proxy

headerSpec :: Spec
headerSpec = describe "Servant.API.Header" $ do
  let expectsInt :: Maybe Int -> Handler NoContent
      expectsInt (Just x) = do
        when (x /= 5) $ error "Expected 5"
        pure NoContent
      expectsInt Nothing = error "Expected an int"

  let expectsString :: Maybe String -> Handler NoContent
      expectsString (Just x) = do
        when (x /= "more from you") $ error "Expected more from you"
        pure NoContent
      expectsString Nothing = error "Expected a string"

  with (pure (serve (headerApi (Proxy :: Proxy Int)) expectsInt)) $ do
    let delete' x = THW.request methodDelete x [("MyHeader", "5")]

    it "passes the header to the handler (Int)" $
      delete' "/" "" `shouldRespondWith` 200

  with (pure (serve (headerApi (Proxy :: Proxy String)) expectsString)) $ do
    let delete' x = THW.request methodDelete x [("MyHeader", "more from you")]

    it "passes the header to the handler (String)" $
      delete' "/" "" `shouldRespondWith` 200

  with (pure (serve (headerApi (Proxy :: Proxy Int)) expectsInt)) $ do
    let delete' x = THW.request methodDelete x [("MyHeader", "not a number")]

    it "checks for parse errors" $
      delete' "/" "" `shouldRespondWith` 400

-- }}}
------------------------------------------------------------------------------

-- * rawSpec {{{

------------------------------------------------------------------------------

type RawApi = "foo" :> Raw

rawApi :: Proxy RawApi
rawApi = Proxy

rawApplication :: Show a => (Request -> a) -> Tagged m Application
rawApplication f = Tagged $ \request_ sendResponse ->
  sendResponse $
    responseLBS
      ok200
      []
      (BSL8.pack $ show $ f request_)

rawSpec :: Spec
rawSpec = describe "Servant.API.Raw" $ do
  it "runs applications" $ flip runSession (serve rawApi (rawApplication (const (42 :: Integer)))) $ do
    response <-
      Network.Wai.Test.request
        defaultRequest
          { pathInfo = ["foo"]
          }
    liftIO $ simpleBody response `shouldBe` "42"

  it "gets the pathInfo modified" $ flip runSession (serve rawApi (rawApplication pathInfo)) $ do
    response <-
      Network.Wai.Test.request
        defaultRequest
          { pathInfo = ["foo", "bar"]
          }
    liftIO $ simpleBody response `shouldBe` BSL8.pack (show ["bar" :: String])

-- }}}
------------------------------------------------------------------------------

-- * rawMSpec {{{

------------------------------------------------------------------------------

type RawMApi = "foo" :> RawM

rawMApi :: Proxy RawMApi
rawMApi = Proxy

rawMServer :: (MonadIO m, Show a) => (Request -> m a) -> ServerT RawMApi m
rawMServer f req resp = liftIO . resp . responseLBS ok200 [] . BSL8.pack . show =<< f req

rawMSpec :: Spec
rawMSpec = describe "Servant.API.RawM" $ do
  it "gives access to monadic context"
    $ flip
      runSession
      ( serve
          rawMApi
          (hoistServer rawMApi (`runReaderT` (42 :: Integer)) (rawMServer (const ask)))
      )
    $ do
      response <-
        Network.Wai.Test.request
          defaultRequest
            { pathInfo = ["foo"]
            }
      liftIO $ simpleBody response `shouldBe` "42"

  it "lets users throw servant errors" $ flip runSession (serve rawMApi (rawMServer (const $ throwError err404 >> pure (42 :: Integer)))) $ do
    response <-
      Network.Wai.Test.request
        defaultRequest
          { pathInfo = ["foo"]
          }
    liftIO $ statusCode (simpleStatus response) `shouldBe` 404

  it "gets the pathInfo modified" $ flip runSession (serve rawMApi (rawMServer (pure . pathInfo))) $ do
    response <-
      Network.Wai.Test.request
        defaultRequest
          { pathInfo = ["foo", "bar"]
          }
    liftIO $ simpleBody response `shouldBe` BSL8.pack (show ["bar" :: String])

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
  pure alice
    :<|> pure jerry
    :<|> pure "a string"
    :<|> pure jerry
    :<|> pure jerry
    :<|> pure NoContent

alternativeSpec :: Spec
alternativeSpec = describe "Servant.API.Alternative" $ with (pure $ serve alternativeApi alternativeServer) $ do
  it "unions endpoints" $ do
    response <- get "/foo"
    liftIO $
      decode' (simpleBody response)
        `shouldBe` Just alice
    response_ <- get "/bar"
    liftIO $
      decode' (simpleBody response_)
        `shouldBe` Just jerry

  it "checks all endpoints before returning 415" $ get "/foo" `shouldRespondWith` 200

  it "returns 404 if the path does not exist" $ get "/nonexistent" `shouldRespondWith` 404

-- }}}
------------------------------------------------------------------------------

-- * responseHeaderSpec {{{

------------------------------------------------------------------------------
type ResponseHeadersApi =
  Get '[JSON] (Headers '[Header "H1" Int, Header "H2" String] String)
    :<|> Post '[JSON] (Headers '[Header "H1" Int, Header "H2" String] String)
    :<|> Put '[JSON] (Headers '[Header "H1" Int, Header "H2" String] String)
    :<|> Patch '[JSON] (Headers '[Header "H1" Int, Header "H2" String] String)

responseHeadersServer :: Server ResponseHeadersApi
responseHeadersServer =
  let h = pure $ addHeader 5 $ addHeader "kilroy" "hi"
   in h :<|> h :<|> h :<|> h

responseHeadersSpec :: Spec
responseHeadersSpec = describe "ResponseHeaders" $ with (pure $ serve (Proxy :: Proxy ResponseHeadersApi) responseHeadersServer) $ do
  let methods = [methodGet, methodPost, methodPut, methodPatch]

  it "includes the headers in the response" $
    forM_ methods $ \method ->
      THW.request method "/" [] ""
        `shouldRespondWith` "\"hi\""
          { matchHeaders = ["H1" <:> "5", "H2" <:> "kilroy"]
          , matchStatus = 200
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

-- * uverbResponseHeaderSpec {{{

------------------------------------------------------------------------------
type UVerbHeaderResponse =
  '[ WithStatus 200 (Headers '[Header "H1" Int] String)
   , WithStatus 404 String
   ]

type UVerbResponseHeadersApi =
  Capture "ok" Bool :> UVerb 'GET '[JSON] UVerbHeaderResponse

uverbResponseHeadersServer :: Server UVerbResponseHeadersApi
uverbResponseHeadersServer True = respond . WithStatus @200 . addHeader @"H1" (5 :: Int) $ ("foo" :: String)
uverbResponseHeadersServer False = respond . WithStatus @404 $ ("bar" :: String)

uverbResponseHeadersSpec :: Spec
uverbResponseHeadersSpec =
  describe "UVerbResponseHeaders" $
    with (pure $ serve (Proxy :: Proxy UVerbResponseHeadersApi) uverbResponseHeadersServer) $
      it "includes the headers in the response" $
        THW.request methodGet "/true" [] ""
          `shouldRespondWith` "\"foo\""
            { matchHeaders = ["H1" <:> "5"]
            , matchStatus = 200
            }

-- }}}
------------------------------------------------------------------------------

-- * miscCombinatorSpec {{{

------------------------------------------------------------------------------
type MiscCombinatorsAPI =
  "version" :> HttpVersion :> Get '[JSON] String
    :<|> "secure" :> IsSecure :> Get '[JSON] String
    :<|> "host" :> RemoteHost :> Get '[JSON] String
    :<|> "empty" :> EmptyAPI

miscApi :: Proxy MiscCombinatorsAPI
miscApi = Proxy

miscServ :: Server MiscCombinatorsAPI
miscServ =
  versionHandler
    :<|> secureHandler
    :<|> hostHandler
    :<|> emptyServer
  where
    versionHandler = pure . show
    secureHandler Secure = pure "secure"
    secureHandler NotSecure = pure "not secure"
    hostHandler = pure . show

miscCombinatorSpec :: Spec
miscCombinatorSpec = with (pure $ serve miscApi miscServ) $
  describe "Misc. combinators for request inspection" $ do
    it "Successfully gets the HTTP version specified in the request" $
      go "/version" "\"HTTP/1.0\""

    it "Checks that hspec-wai uses HTTP, not HTTPS" $
      go "/secure" "\"not secure\""

    it "Checks that hspec-wai issues request from 0.0.0.0" $
      go "/host" "\"0.0.0.0:0\""

    it "Doesn't serve anything from the empty API" $
      Test.Hspec.Wai.get "empty" `shouldRespondWith` 404
  where
    go path res = Test.Hspec.Wai.get path `shouldRespondWith` res

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
  const (pure jerry)
    :<|> Tagged (\_ sendResponse -> sendResponse $ responseLBS imATeapot418 [] "")

basicAuthContext :: Context '[BasicAuthCheck ()]
basicAuthContext =
  let basicHandler = BasicAuthCheck $ \(BasicAuthData usr pass) ->
        if usr == "servant" && pass == "server"
          then pure (Authorized ())
          else pure Unauthorized
   in basicHandler :. EmptyContext

basicAuthSpec :: Spec
basicAuthSpec = describe "Servant.API.BasicAuth" $ with (pure (serveWithContext basicAuthApi basicAuthContext basicAuthServer)) $ context "Basic Authentication" $ do
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

type GenAuthAPI =
  AuthProtect "auth" :> "auth" :> Get '[JSON] Animal
    :<|> Raw

genAuthApi :: Proxy GenAuthAPI
genAuthApi = Proxy

genAuthServer :: Server GenAuthAPI
genAuthServer =
  const (pure tweety)
    :<|> Tagged (\_ sendResponse -> sendResponse $ responseLBS imATeapot418 [] "")

type instance AuthServerData (AuthProtect "auth") = ()

genAuthContext :: Context '[AuthHandler Request ()]
genAuthContext =
  let authHandler req = case lookup "Auth" (requestHeaders req) of
        Just "secret" -> pure ()
        Just _ -> throwError err403
        Nothing -> throwError err401
   in mkAuthHandler authHandler :. EmptyContext

genAuthSpec :: Spec
genAuthSpec = describe "Servant.API.Auth" $ with (pure (serveWithContext genAuthApi genAuthContext genAuthServer)) $ context "Custom Auth Protection" $ do
  it "returns 401 when missing headers" $ do
    get "/auth" `shouldRespondWith` 401

  it "returns 403 on wrong passwords" $ do
    THW.request methodGet "/auth" [("Auth", "wrong")] "" `shouldRespondWith` 403

  it "returns 200 with the right header" $ do
    THW.request methodGet "/auth" [("Auth", "secret")] "" `shouldRespondWith` 200

  it "plays nice with subsequent Raw endpoints" $ do
    get "/foo" `shouldRespondWith` 418

-- }}}
------------------------------------------------------------------------------

-- * UVerb {{{

------------------------------------------------------------------------------

newtype PersonResponse = PersonResponse Person
  deriving (Generic)

instance ToJSON PersonResponse

instance HasStatus PersonResponse where
  type StatusOf PersonResponse = 200

newtype RedirectResponse = RedirectResponse String
  deriving (Generic)

instance ToJSON RedirectResponse

instance HasStatus RedirectResponse where
  type StatusOf RedirectResponse = 301

newtype AnimalResponse = AnimalResponse Animal
  deriving (Generic)

instance ToJSON AnimalResponse

instance HasStatus AnimalResponse where
  type StatusOf AnimalResponse = 203

type UVerbApi =
  "person" :> Capture "shouldRedirect" Bool :> UVerb 'GET '[JSON] '[PersonResponse, RedirectResponse]
    :<|> "animal" :> UVerb 'GET '[JSON] '[AnimalResponse]

uverbSpec :: Spec
uverbSpec = describe "Servant.API.UVerb " $ do
  let
    joe = Person "joe" 42
    mouse = Animal "Mouse" 7

    personHandler
      :: Bool
      -> Handler
           ( Union
               '[ PersonResponse
                , RedirectResponse
                ]
           )
    personHandler True = respond $ RedirectResponse "over there!"
    personHandler False = respond $ PersonResponse joe

    animalHandler = respond $ AnimalResponse mouse

    server :: Server UVerbApi
    server = personHandler :<|> animalHandler

  with (pure $ serve (Proxy :: Proxy UVerbApi) server) $ do
    context "A route returning either 301/String or 200/Person" $ do
      context "when requesting the person" $ do
        let theRequest = THW.get "/person/false"
        it "returns status 200" $
          theRequest `shouldRespondWith` 200
        it "returns a person" $ do
          response <- theRequest
          liftIO $ decode' (simpleBody response) `shouldBe` Just joe
      context "requesting the redirect" $
        it "returns a message and status 301" $
          THW.get "/person/true"
            `shouldRespondWith` "\"over there!\""{matchStatus = 301}
    context "a route with a single response type" $ do
      let theRequest = THW.get "/animal"
      it "should return the defined status code" $
        theRequest `shouldRespondWith` 203
      it "should return the expected response" $ do
        response <- theRequest
        liftIO $ decode' (simpleBody response) `shouldBe` Just mouse

-- }}}
------------------------------------------------------------------------------

-- * Test data types {{{

------------------------------------------------------------------------------

data Person = Person
  { name :: String
  , age :: Integer
  }
  deriving (Eq, Generic, Show)

instance ToJSON Person

instance FromJSON Person

alice :: Person
alice = Person "Alice" 42

data Animal = Animal
  { species :: String
  , numberOfLegs :: Integer
  }
  deriving (Eq, Generic, Show)

instance ToJSON Animal

instance FromJSON Animal

jerry :: Animal
jerry = Animal "Mouse" 4

tweety :: Animal
tweety = Animal "Bird" 2

-- weird animal with non-integer amount of ears
chimera :: Animal
chimera = Animal "Chimera" (-1)

beholder :: Animal
beholder = Animal "Beholder" 0

-- }}}
