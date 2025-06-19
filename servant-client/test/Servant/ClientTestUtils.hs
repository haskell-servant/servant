{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -freduction-depth=100 #-}

module Servant.ClientTestUtils where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Monad (join)
import Control.Monad.Error.Class (throwError)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Char (chr, isPrint)
import Data.Maybe (fromMaybe)
import Data.Monoid ()
import Data.Proxy
import Data.SOP
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import qualified Generics.SOP as GSOP
import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Types as HTTP
import Network.Socket
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp
import Prelude.Compat
import Servant.API
  ( AuthProtect
  , BasicAuth
  , BasicAuthData (..)
  , Capture
  , CaptureAll
  , DeepQuery
  , DeleteNoContent
  , EmptyAPI
  , FormUrlEncoded
  , Fragment
  , FromHttpApiData (..)
  , Get
  , Header
  , Headers
  , Host
  , JSON
  , MimeRender (mimeRender)
  , MimeUnrender (mimeUnrender)
  , NamedRoutes
  , NoContent (NoContent)
  , PlainText
  , Post
  , QueryFlag
  , QueryParam
  , QueryParams
  , QueryString
  , Raw
  , ReqBody
  , StdMethod (GET)
  , ToHttpApiData (..)
  , UVerb
  , Union
  , Verb
  , WithStatus (WithStatus)
  , addHeader
  , (:<|>) ((:<|>))
  , (:>)
  )
import Servant.API.Generic ((:-))
import Servant.API.MultiVerb
import Servant.API.QueryString (FromDeepQuery (..), ToDeepQuery (..))
import Servant.API.Range
import qualified Servant.Client.Core.Auth as Auth
import Servant.Server
import Servant.Server.Experimental.Auth
import Servant.Test.ComprehensiveAPI
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck
import Text.Read (readMaybe)
import Web.FormUrlEncoded (FromForm, ToForm)
import Prelude ()

import Servant.Client

-- This declaration simply checks that all instances are in place.
_ = client comprehensiveAPIWithoutStreaming

-- * test data types

data Person = Person
  { _name :: String
  , _age :: Integer
  }
  deriving (Eq, Generic, Read, Show)

instance ToJSON Person

instance FromJSON Person

instance ToForm Person

instance FromForm Person

instance Arbitrary Person where
  arbitrary = Person <$> arbitrary <*> arbitrary

instance MimeRender PlainText Person where
  mimeRender _ = LazyByteString.fromStrict . encodeUtf8 . Text.pack . show

instance MimeUnrender PlainText Person where
  mimeUnrender _ =
    -- This does not handle any errors, but it should be fine for tests
    Right . read . Text.unpack . decodeUtf8 . LazyByteString.toStrict

alice :: Person
alice = Person "Alice" 42

carol :: Person
carol = Person "Carol" 17

type TestHeaders = '[Header "X-Example1" Int, Header "X-Example2" String]

type TestSetCookieHeaders = '[Header "Set-Cookie" String, Header "Set-Cookie" String]

data RecordRoutes mode = RecordRoutes
  { version :: mode :- "version" :> Get '[JSON] Int
  , echo :: mode :- "echo" :> Capture "string" String :> Get '[JSON] String
  , otherRoutes :: mode :- "other" :> Capture "someParam" Int :> NamedRoutes OtherRoutes
  }
  deriving (Generic)

newtype OtherRoutes mode = OtherRoutes
  { something :: mode :- "something" :> Get '[JSON] [String]
  }
  deriving (Generic)

-- Get for HTTP 307 Temporary Redirect
type Get307 = Verb 'GET 307

data Filter = Filter
  { ageFilter :: Integer
  , nameFilter :: String
  }
  deriving (Show)

instance FromDeepQuery Filter where
  fromDeepQuery params = do
    let maybeToRight l = maybe (Left l) Right
    age' <- maybeToRight "missing age" $ readMaybe . Text.unpack =<< join (lookup ["age"] params)
    name' <- maybeToRight "missing name" $ join $ lookup ["name"] params
    return $ Filter age' (Text.unpack name')

instance ToDeepQuery Filter where
  toDeepQuery (Filter age' name') =
    [ (["age"], Just (Text.pack $ show age'))
    , (["name"], Just (Text.pack name'))
    ]

-----------------------------
-- MultiVerb test endpoint --
-----------------------------

-- This is the list of all possible responses
type MultipleChoicesIntResponses =
  '[ RespondEmpty 400 "Negative"
   , Respond 200 "Even number" Bool
   , Respond 200 "Odd number" Int
   ]

data MultipleChoicesIntResult
  = NegativeNumber
  | Even Bool
  | Odd Int
  deriving stock (Generic)
  deriving
    (AsUnion MultipleChoicesIntResponses)
    via GenericAsUnion MultipleChoicesIntResponses MultipleChoicesIntResult

instance GSOP.Generic MultipleChoicesIntResult

-- This is our endpoint description
type MultipleChoicesInt =
  Capture "int" Int
    :> MultiVerb
         'GET
         '[JSON]
         MultipleChoicesIntResponses
         MultipleChoicesIntResult

type Api =
  Get '[JSON] Person
    :<|> "get" :> Get '[JSON] Person
    -- This endpoint returns a response with status code 307 Temporary Redirect,
    -- different from the ones in the 2xx successful class, to test derivation
    -- of clients' api.
    :<|> "get307" :> Get307 '[PlainText] Text
    :<|> "deleteEmpty" :> DeleteNoContent
    :<|> "capture" :> Capture "name" String :> Get '[JSON, FormUrlEncoded] Person
    :<|> "captureAll" :> CaptureAll "names" String :> Get '[JSON] [Person]
    :<|> "body" :> ReqBody '[FormUrlEncoded, JSON] Person :> Post '[JSON] Person
    :<|> "param" :> QueryParam "name" String :> Get '[FormUrlEncoded, JSON] Person
    -- This endpoint makes use of a 'Raw' server because it is not currently
    -- possible to handle arbitrary binary query param values with
    -- @servant-server@
    :<|> "param-binary" :> QueryParam "payload" UrlEncodedByteString :> Raw
    :<|> "params" :> QueryParams "names" String :> Get '[JSON] [Person]
    :<|> "flag" :> QueryFlag "flag" :> Get '[JSON] Bool
    :<|> "query-string" :> QueryString :> Get '[JSON] Person
    :<|> "deep-query" :> DeepQuery "filter" Filter :> Get '[JSON] Person
    :<|> "fragment" :> Fragment String :> Get '[JSON] Person
    :<|> "rawSuccess" :> Raw
    :<|> "rawSuccessPassHeaders" :> Raw
    :<|> "rawFailure" :> Raw
    :<|> "multiple"
      :> Capture "first" String
      :> QueryParam "second" Int
      :> QueryFlag "third"
      :> ReqBody '[JSON] [(String, [Rational])]
      :> Get '[JSON] (String, Maybe Int, Bool, [(String, [Rational])])
    :<|> "headers" :> Get '[JSON] (Headers TestHeaders Bool)
    :<|> "uverb-headers" :> UVerb 'GET '[JSON] '[WithStatus 200 (Headers TestHeaders Bool), WithStatus 204 String]
    :<|> "set-cookie-headers" :> Get '[JSON] (Headers TestSetCookieHeaders Bool)
    :<|> "deleteContentType" :> DeleteNoContent
    :<|> "redirectWithCookie" :> Raw
    :<|> "empty" :> EmptyAPI
    :<|> "uverb-success-or-redirect"
      :> Capture "bool" Bool
      :> UVerb
           'GET
           '[PlainText]
           '[ WithStatus 200 Person
            , WithStatus 301 Text
            ]
    :<|> "uverb-get-created" :> UVerb 'GET '[PlainText] '[WithStatus 201 Person]
    :<|> NamedRoutes RecordRoutes
    :<|> "multiple-choices-int" :> MultipleChoicesInt
    :<|> "captureVerbatim" :> Capture "someString" Verbatim :> Get '[PlainText] Text
    :<|> "host-test" :> Host "servant.example" :> Get '[JSON] Bool
    :<|> PaginatedAPI

api :: Proxy Api
api = Proxy

getRoot :: ClientM Person
getGet :: ClientM Person
getGet307 :: ClientM Text
getDeleteEmpty :: ClientM NoContent
getCapture :: String -> ClientM Person
getCaptureAll :: [String] -> ClientM [Person]
getBody :: Person -> ClientM Person
getQueryParam :: Maybe String -> ClientM Person
getQueryParamBinary :: Maybe UrlEncodedByteString -> HTTP.Method -> ClientM Response
getQueryParams :: [String] -> ClientM [Person]
getQueryFlag :: Bool -> ClientM Bool
getQueryString :: [(ByteString, Maybe ByteString)] -> ClientM Person
getDeepQuery :: Filter -> ClientM Person
getFragment :: ClientM Person
getRawSuccess :: HTTP.Method -> ClientM Response
getRawSuccessPassHeaders :: HTTP.Method -> ClientM Response
getRawFailure :: HTTP.Method -> ClientM Response
getMultiple
  :: String
  -> Maybe Int
  -> Bool
  -> [(String, [Rational])]
  -> ClientM (String, Maybe Int, Bool, [(String, [Rational])])
getRespHeaders :: ClientM (Headers TestHeaders Bool)
getUVerbRespHeaders :: ClientM (Union '[WithStatus 200 (Headers TestHeaders Bool), WithStatus 204 String])
getSetCookieHeaders :: ClientM (Headers TestSetCookieHeaders Bool)
getDeleteContentType :: ClientM NoContent
getRedirectWithCookie :: HTTP.Method -> ClientM Response
uverbGetSuccessOrRedirect
  :: Bool
  -> ClientM
       ( Union
           '[ WithStatus 200 Person
            , WithStatus 301 Text
            ]
       )
uverbGetCreated :: ClientM (Union '[WithStatus 201 Person])
recordRoutes :: RecordRoutes (AsClientT ClientM)
multiChoicesInt :: Int -> ClientM MultipleChoicesIntResult
captureVerbatim :: Verbatim -> ClientM Text
getHost :: ClientM Bool
getPaginatedPerson :: Maybe (Range 1 100) -> ClientM [Person]
getRoot
  :<|> getGet
  :<|> getGet307
  :<|> getDeleteEmpty
  :<|> getCapture
  :<|> getCaptureAll
  :<|> getBody
  :<|> getQueryParam
  :<|> getQueryParamBinary
  :<|> getQueryParams
  :<|> getQueryFlag
  :<|> getQueryString
  :<|> getDeepQuery
  :<|> getFragment
  :<|> getRawSuccess
  :<|> getRawSuccessPassHeaders
  :<|> getRawFailure
  :<|> getMultiple
  :<|> getRespHeaders
  :<|> getUVerbRespHeaders
  :<|> getSetCookieHeaders
  :<|> getDeleteContentType
  :<|> getRedirectWithCookie
  :<|> EmptyClient
  :<|> uverbGetSuccessOrRedirect
  :<|> uverbGetCreated
  :<|> recordRoutes
  :<|> multiChoicesInt
  :<|> captureVerbatim
  :<|> getHost
  :<|> getPaginatedPerson = client api

server :: Application
server =
  serve
    api
    ( return carol
        :<|> return alice
        :<|> return "redirecting"
        :<|> return NoContent
        :<|> (\name -> return $ Person name 0)
        :<|> (\names -> return (zipWith Person names [0 ..]))
        :<|> return
        :<|> ( \case
                 Just "alice" -> return alice
                 Just n -> throwError $ ServerError 400 (n ++ " not found") "" []
                 Nothing -> throwError $ ServerError 400 "missing parameter" "" []
             )
        :<|> const
          ( Tagged $ \request respond ->
              respond
                . maybe
                  (Wai.responseLBS HTTP.notFound404 [] "Missing: payload")
                  (Wai.responseLBS HTTP.ok200 [] . LazyByteString.fromStrict)
                . join
                . lookup "payload"
                $ Wai.queryString request
          )
        :<|> (\names -> return (zipWith Person names [0 ..]))
        :<|> return
        :<|> ( \q ->
                 return
                   alice
                     { _name = maybe mempty C8.unpack $ join (lookup "name" q)
                     , _age = fromMaybe 0 (readMaybe . C8.unpack =<< join (lookup "age" q))
                     }
             )
        :<|> ( \filter' ->
                 return
                   alice
                     { _name = nameFilter filter'
                     , _age = ageFilter filter'
                     }
             )
        :<|> return alice
        :<|> Tagged (\_request respond -> respond $ Wai.responseLBS HTTP.ok200 [] "rawSuccess")
        :<|> Tagged (\request respond -> respond $ Wai.responseLBS HTTP.ok200 (Wai.requestHeaders request) "rawSuccess")
        :<|> Tagged (\_request respond -> respond $ Wai.responseLBS HTTP.badRequest400 [] "rawFailure")
        :<|> (\a b c d -> return (a, b, c, d))
        :<|> return (addHeader 1729 $ addHeader "eg2" True)
        :<|> (pure . Z . I . WithStatus $ addHeader 1729 $ addHeader "eg2" True)
        :<|> return (addHeader "cookie1" $ addHeader "cookie2" True)
        :<|> return NoContent
        :<|> Tagged (\_request respond -> respond $ Wai.responseLBS HTTP.found302 [("Location", "testlocation"), ("Set-Cookie", "testcookie=test")] "")
        :<|> emptyServer
        :<|> ( \shouldRedirect ->
                 if shouldRedirect
                   then respond (WithStatus @301 ("redirecting" :: Text))
                   else respond (WithStatus @200 alice)
             )
        :<|> respond (WithStatus @201 carol)
        :<|> RecordRoutes
          { version = pure 42
          , echo = pure
          , otherRoutes =
              const
                OtherRoutes
                  { something = pure ["foo", "bar", "pweet"]
                  }
          }
        :<|> ( \param ->
                 if param < 0
                   then pure NegativeNumber
                   else
                     if even param
                       then pure $ Odd 3
                       else pure $ Even True
             )
        :<|> pure . decodeUtf8 . unVerbatim
        :<|> pure True
        :<|> usersServer
    )

-- * api for testing failures

type FailApi =
  "get" :> Raw
    :<|> "capture" :> Capture "name" String :> Raw
    :<|> "body" :> Raw
    :<|> "headers" :> Raw

failApi :: Proxy FailApi
failApi = Proxy

failServer :: Application
failServer =
  serve
    failApi
    ( Tagged (\_request respond -> respond $ Wai.responseLBS HTTP.ok200 [] "")
        :<|> (\_capture -> Tagged $ \_request respond -> respond $ Wai.responseLBS HTTP.ok200 [("content-type", "application/json")] "")
        :<|> Tagged (\_request respond -> respond $ Wai.responseLBS HTTP.ok200 [("content-type", "fooooo")] "")
        :<|> Tagged (\_request respond -> respond $ Wai.responseLBS HTTP.ok200 [("content-type", "application/x-www-form-urlencoded"), ("X-Example1", "1"), ("X-Example2", "foo")] "")
    )

-- * basic auth stuff

type BasicAuthAPI =
  BasicAuth "foo-realm" () :> "private" :> "basic" :> Get '[JSON] Person

basicAuthAPI :: Proxy BasicAuthAPI
basicAuthAPI = Proxy

basicAuthHandler :: BasicAuthCheck ()
basicAuthHandler =
  let check (BasicAuthData username password) =
        if username == "servant" && password == "server"
          then return (Authorized ())
          else return Unauthorized
   in BasicAuthCheck check

basicServerContext :: Context '[BasicAuthCheck ()]
basicServerContext = basicAuthHandler :. EmptyContext

basicAuthServer :: Application
basicAuthServer = serveWithContext basicAuthAPI basicServerContext (const (return alice))

-- * general auth stuff

type GenAuthAPI =
  AuthProtect "auth-tag" :> "private" :> "auth" :> Get '[JSON] Person

genAuthAPI :: Proxy GenAuthAPI
genAuthAPI = Proxy

type instance AuthServerData (AuthProtect "auth-tag") = ()

type instance Auth.AuthClientData (AuthProtect "auth-tag") = ()

genAuthHandler :: AuthHandler Wai.Request ()
genAuthHandler =
  let handler req = case lookup "AuthHeader" (Wai.requestHeaders req) of
        Nothing -> throwError (err401{errBody = "Missing auth header"})
        Just _ -> return ()
   in mkAuthHandler handler

genAuthServerContext :: Context '[AuthHandler Wai.Request ()]
genAuthServerContext = genAuthHandler :. EmptyContext

genAuthServer :: Application
genAuthServer = serveWithContext genAuthAPI genAuthServerContext (const (return alice))

{-# NOINLINE manager' #-}
manager' :: C.Manager
manager' = unsafePerformIO $ C.newManager C.defaultManagerSettings

runClient :: ClientM a -> BaseUrl -> IO (Either ClientError a)
runClient x baseUrl' = runClientM x (mkClientEnv manager' baseUrl')

-- * utils

startWaiApp :: Application -> IO (ThreadId, BaseUrl)
startWaiApp app = do
  (port, socket) <- openTestSocket
  let settings = setPort port defaultSettings
  thread <- forkIO $ runSettingsSocket settings socket app
  return (thread, BaseUrl Http "localhost" port "")

endWaiApp :: (ThreadId, BaseUrl) -> IO ()
endWaiApp (thread, _) = killThread thread

openTestSocket :: IO (Port, Socket)
openTestSocket = do
  s <- socket AF_INET Stream defaultProtocol
  let localhost = tupleToHostAddress (127, 0, 0, 1)
  bind s (SockAddrInet defaultPort localhost)
  listen s 1
  port <- socketPort s
  return (fromIntegral port, s)

pathGen :: Gen (NonEmptyList Char)
pathGen = fmap NonEmpty path
  where
    path =
      listOf1 $
        elements $
          filter (not . (`elem` ("?%[]/#;" :: String))) $
            filter isPrint $
              map chr [0 .. 127]

newtype UrlEncodedByteString = UrlEncodedByteString {unUrlEncodedByteString :: ByteString}

instance ToHttpApiData UrlEncodedByteString where
  toEncodedUrlPiece = byteString . HTTP.urlEncode True . unUrlEncodedByteString
  toUrlPiece = decodeUtf8 . HTTP.urlEncode True . unUrlEncodedByteString

instance FromHttpApiData UrlEncodedByteString where
  parseUrlPiece = pure . UrlEncodedByteString . HTTP.urlDecode True . encodeUtf8

newtype Verbatim = Verbatim {unVerbatim :: ByteString}

instance ToHttpApiData Verbatim where
  toEncodedUrlPiece = byteString . unVerbatim
  toUrlPiece = decodeUtf8 . unVerbatim

instance FromHttpApiData Verbatim where
  parseUrlPiece = pure . Verbatim . encodeUtf8

-- * range type example

type PaginatedAPI =
  "users" :> QueryParam "page" (Range 1 100) :> Get '[JSON] [Person]

usersServer :: Maybe (Range 1 100) -> Handler [Person]
usersServer mpage = do
  let pageNum = maybe 1 unRange mpage
  -- pageNum is guaranteed to be between 1 and 100
  return [Person "Example" $ fromIntegral pageNum]
