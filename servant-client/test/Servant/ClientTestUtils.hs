{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -freduction-depth=100 #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Servant.ClientTestUtils where

import           Prelude ()
import           Prelude.Compat

import           Control.Concurrent
                 (ThreadId, forkIO, killThread)
import           Control.Monad.Error.Class
                 (throwError)
import           Data.Aeson
import qualified Data.ByteString.Lazy as LazyByteString
import           Data.Char
                 (chr, isPrint)
import           Data.Monoid ()
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)
import           GHC.Generics
                 (Generic)
import qualified Network.HTTP.Client                  as C
import qualified Network.HTTP.Types                   as HTTP
import           Network.Socket
import qualified Network.Wai                          as Wai
import           Network.Wai.Handler.Warp
import           System.IO.Unsafe
                 (unsafePerformIO)
import           Test.QuickCheck
import           Web.FormUrlEncoded
                 (FromForm, ToForm)

import           Servant.API
                 ((:<|>) ((:<|>)), (:>), AuthProtect, BasicAuth,
                 BasicAuthData (..), Capture, CaptureAll,
                 DeleteNoContent, EmptyAPI, FormUrlEncoded, Get, Header,
                 Headers, JSON, MimeRender(mimeRender),
                 MimeUnrender(mimeUnrender), NoContent (NoContent), PlainText,
                 Post, QueryFlag, QueryParam, QueryParams, Raw, ReqBody,
                 StdMethod(GET), Union, UVerb, WithStatus(WithStatus),
                 addHeader)
import           Servant.Client
import qualified Servant.Client.Core.Auth    as Auth
import           Servant.Server
import           Servant.Server.Experimental.Auth
import           Servant.Test.ComprehensiveAPI

-- This declaration simply checks that all instances are in place.
_ = client comprehensiveAPIWithoutStreaming

-- * test data types

data Person = Person
  { _name :: String
  , _age  :: Integer
  } deriving (Eq, Show, Read, Generic)

instance ToJSON Person
instance FromJSON Person

instance ToForm Person
instance FromForm Person

instance Arbitrary Person where
  arbitrary = Person <$> arbitrary <*> arbitrary

instance MimeRender PlainText Person where
  mimeRender _ = LazyByteString.fromStrict . encodeUtf8 . Text.pack . show

instance MimeRender ctype a => MimeRender ctype (WithStatus _status a) where
  mimeRender contentTypeProxy (WithStatus a) = mimeRender contentTypeProxy a

instance MimeUnrender ctype a => MimeUnrender ctype (WithStatus _status a) where
  mimeUnrender contentTypeProxy input =
    WithStatus <$> mimeUnrender contentTypeProxy input

instance MimeUnrender PlainText Person where
  mimeUnrender _ =
    -- This does not handle any errors, but it should be fine for tests
    Right . read . Text.unpack . decodeUtf8 . LazyByteString.toStrict


alice :: Person
alice = Person "Alice" 42

carol :: Person
carol = Person "Carol" 17

type TestHeaders = '[Header "X-Example1" Int, Header "X-Example2" String]

type Api =
  Get '[JSON] Person
  :<|> "get" :> Get '[JSON] Person
  :<|> "deleteEmpty" :> DeleteNoContent
  :<|> "capture" :> Capture "name" String :> Get '[JSON,FormUrlEncoded] Person
  :<|> "captureAll" :> CaptureAll "names" String :> Get '[JSON] [Person]
  :<|> "body" :> ReqBody '[FormUrlEncoded,JSON] Person :> Post '[JSON] Person
  :<|> "param" :> QueryParam "name" String :> Get '[FormUrlEncoded,JSON] Person
  :<|> "params" :> QueryParams "names" String :> Get '[JSON] [Person]
  :<|> "flag" :> QueryFlag "flag" :> Get '[JSON] Bool
  :<|> "rawSuccess" :> Raw
  :<|> "rawSuccessPassHeaders" :> Raw
  :<|> "rawFailure" :> Raw
  :<|> "multiple" :>
            Capture "first" String :>
            QueryParam "second" Int :>
            QueryFlag "third" :>
            ReqBody '[JSON] [(String, [Rational])] :>
            Get '[JSON] (String, Maybe Int, Bool, [(String, [Rational])])
  :<|> "headers" :> Get '[JSON] (Headers TestHeaders Bool)
  :<|> "deleteContentType" :> DeleteNoContent
  :<|> "redirectWithCookie" :> Raw
  :<|> "empty" :> EmptyAPI
  :<|> "uverb-success-or-redirect" :>
            Capture "bool" Bool :>
            UVerb 'GET '[PlainText] '[WithStatus 200 Person,
                                      WithStatus 301 Text]
  :<|> "uverb-get-created" :> UVerb 'GET '[PlainText] '[WithStatus 201 Person]


api :: Proxy Api
api = Proxy

getRoot         :: ClientM Person
getGet          :: ClientM Person
getDeleteEmpty  :: ClientM NoContent
getCapture      :: String -> ClientM Person
getCaptureAll   :: [String] -> ClientM [Person]
getBody         :: Person -> ClientM Person
getQueryParam   :: Maybe String -> ClientM Person
getQueryParams  :: [String] -> ClientM [Person]
getQueryFlag    :: Bool -> ClientM Bool
getRawSuccess   :: HTTP.Method -> ClientM Response
getRawSuccessPassHeaders :: HTTP.Method -> ClientM Response
getRawFailure   :: HTTP.Method -> ClientM Response
getMultiple     :: String -> Maybe Int -> Bool -> [(String, [Rational])]
  -> ClientM (String, Maybe Int, Bool, [(String, [Rational])])
getRespHeaders  :: ClientM (Headers TestHeaders Bool)
getDeleteContentType :: ClientM NoContent
getRedirectWithCookie :: HTTP.Method -> ClientM Response
uverbGetSuccessOrRedirect :: Bool
                          -> ClientM (Union '[WithStatus 200 Person,
                                              WithStatus 301 Text])
uverbGetCreated :: ClientM (Union '[WithStatus 201 Person])

getRoot
  :<|> getGet
  :<|> getDeleteEmpty
  :<|> getCapture
  :<|> getCaptureAll
  :<|> getBody
  :<|> getQueryParam
  :<|> getQueryParams
  :<|> getQueryFlag
  :<|> getRawSuccess
  :<|> getRawSuccessPassHeaders
  :<|> getRawFailure
  :<|> getMultiple
  :<|> getRespHeaders
  :<|> getDeleteContentType
  :<|> getRedirectWithCookie
  :<|> EmptyClient
  :<|> uverbGetSuccessOrRedirect
  :<|> uverbGetCreated = client api

server :: Application
server = serve api (
       return carol
  :<|> return alice
  :<|> return NoContent
  :<|> (\ name -> return $ Person name 0)
  :<|> (\ names -> return (zipWith Person names [0..]))
  :<|> return
  :<|> (\ name -> case name of
                   Just "alice" -> return alice
                   Just n -> throwError $ ServerError 400 (n ++ " not found") "" []
                   Nothing -> throwError $ ServerError 400 "missing parameter" "" [])
  :<|> (\ names -> return (zipWith Person names [0..]))
  :<|> return
  :<|> (Tagged $ \ _request respond -> respond $ Wai.responseLBS HTTP.ok200 [] "rawSuccess")
  :<|> (Tagged $ \ request respond -> (respond $ Wai.responseLBS HTTP.ok200 (Wai.requestHeaders $ request) "rawSuccess"))
  :<|> (Tagged $ \ _request respond -> respond $ Wai.responseLBS HTTP.badRequest400 [] "rawFailure")
  :<|> (\ a b c d -> return (a, b, c, d))
  :<|> (return $ addHeader 1729 $ addHeader "eg2" True)
  :<|> return NoContent
  :<|> (Tagged $ \ _request respond -> respond $ Wai.responseLBS HTTP.found302 [("Location", "testlocation"), ("Set-Cookie", "testcookie=test")] "")
  :<|> emptyServer
  :<|> (\shouldRedirect -> if shouldRedirect
                              then respond (WithStatus @301 ("redirecting" :: Text))
                              else respond (WithStatus @200 alice ))
  :<|> respond (WithStatus @201 carol)
  )

type FailApi =
       "get" :> Raw
  :<|> "capture" :> Capture "name" String :> Raw
  :<|> "body" :> Raw
  :<|> "headers" :> Raw
failApi :: Proxy FailApi
failApi = Proxy

failServer :: Application
failServer = serve failApi (
       (Tagged $ \ _request respond -> respond $ Wai.responseLBS HTTP.ok200 [] "")
  :<|> (\ _capture -> Tagged $ \_request respond -> respond $ Wai.responseLBS HTTP.ok200 [("content-type", "application/json")] "")
  :<|> (Tagged $ \_request respond -> respond $ Wai.responseLBS HTTP.ok200 [("content-type", "fooooo")] "")
  :<|> (Tagged $ \_request respond -> respond $ Wai.responseLBS HTTP.ok200 [("content-type", "application/x-www-form-urlencoded"), ("X-Example1", "1"), ("X-Example2", "foo")] "")
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

basicServerContext :: Context '[ BasicAuthCheck () ]
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
        Nothing -> throwError (err401 { errBody = "Missing auth header" })
        Just _ -> return ()
  in mkAuthHandler handler

genAuthServerContext :: Context '[ AuthHandler Wai.Request () ]
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
  path = listOf1 $ elements $
    filter (not . (`elem` ("?%[]/#;" :: String))) $
    filter isPrint $
    map chr [0..127]
