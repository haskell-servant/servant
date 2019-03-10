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
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -freduction-depth=100 #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Servant.TestUtils where

import           Prelude ()
import           Prelude.Compat

import           Control.Concurrent
                 (ThreadId, forkIO, killThread)
import           Control.Monad.Error.Class
                 (throwError)
import           Data.Aeson
import           Data.Char
                 (chr, isPrint)
import           Data.Proxy
import           GHC.Generics
                 (Generic)
import qualified Generics.SOP as SOP
import qualified Network.HTTP.Client                        as C
import qualified Network.HTTP.Types                         as HTTP
import           Network.Socket
import qualified Network.Wai                                as Wai
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
                 Headers, JSON, NoContent (NoContent), Post, QueryFlag,
                 QueryParam, QueryParams, Raw, ReqBody, addHeader)
import           Servant.Client
import qualified Servant.Client.Core.Internal.Auth          as Auth
import           Servant.Server
import           Servant.Server.Experimental.Auth

-- * test data types

data Person = Person
  { _name :: String
  , _age  :: Integer
  } deriving (Eq, Show, Generic)

instance ToJSON Person
instance FromJSON Person

instance ToForm Person
instance FromForm Person

instance Arbitrary Person where
  arbitrary = Person <$> arbitrary <*> arbitrary

alice :: Person
alice = Person "Alice" 42

carol :: Person
carol = Person "Carol" 17

type TestHeaders = '[Header "X-Example1" Int, Header "X-Example2" String]

type Api =
  Get '[JSON] Person
  :<|> "get" :> Get '[JSON] Person
  :<|> "deleteEmpty" :> DeleteNoContent '[JSON] NoContent
  :<|> "capture" :> Capture "name" String :> Get '[JSON,FormUrlEncoded] Person
  :<|> "captureAll" :> CaptureAll "names" String :> Get '[JSON] [Person]
  :<|> "body" :> ReqBody '[FormUrlEncoded,JSON] Person :> Post '[JSON] Person
  :<|> "param" :> QueryParam "name" String :> Get '[FormUrlEncoded,JSON] Person
  :<|> "params" :> QueryParams "names" String :> Get '[JSON] [Person]
  :<|> "flag" :> QueryFlag "flag" :> Get '[JSON] Bool
  :<|> "rawSuccess" :> Raw
  :<|> "rawFailure" :> Raw
  :<|> "multiple" :>
            Capture "first" String :>
            QueryParam "second" Int :>
            QueryFlag "third" :>
            ReqBody '[JSON] [(String, [Rational])] :>
            Get '[JSON] (String, Maybe Int, Bool, [(String, [Rational])])
  :<|> "headers" :> Get '[JSON] (Headers TestHeaders Bool)
  :<|> "deleteContentType" :> DeleteNoContent '[JSON] NoContent
  :<|> "empty" :> EmptyAPI

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
getRawFailure   :: HTTP.Method -> ClientM Response
getMultiple     :: String -> Maybe Int -> Bool -> [(String, [Rational])]
  -> ClientM (String, Maybe Int, Bool, [(String, [Rational])])
getRespHeaders  :: ClientM (Headers TestHeaders Bool)
getDeleteContentType :: ClientM NoContent

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
  :<|> getRawFailure
  :<|> getMultiple
  :<|> getRespHeaders
  :<|> getDeleteContentType
  :<|> EmptyClient = client api

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
                   Just n -> throwError $ ServantErr 400 (n ++ " not found") "" []
                   Nothing -> throwError $ ServantErr 400 "missing parameter" "" [])
  :<|> (\ names -> return (zipWith Person names [0..]))
  :<|> return
  :<|> (Tagged $ \ _request respond -> respond $ Wai.responseLBS HTTP.ok200 [] "rawSuccess")
  :<|> (Tagged $ \ _request respond -> respond $ Wai.responseLBS HTTP.badRequest400 [] "rawFailure")
  :<|> (\ a b c d -> return (a, b, c, d))
  :<|> (return $ addHeader 1729 $ addHeader "eg2" True)
  :<|> return NoContent
  :<|> emptyServer)


type FailApi =
       "get" :> Raw
  :<|> "capture" :> Capture "name" String :> Raw
  :<|> "body" :> Raw
failApi :: Proxy FailApi
failApi = Proxy

failServer :: Application
failServer = serve failApi (
       (Tagged $ \ _request respond -> respond $ Wai.responseLBS HTTP.ok200 [] "")
  :<|> (\ _capture -> Tagged $ \_request respond -> respond $ Wai.responseLBS HTTP.ok200 [("content-type", "application/json")] "")
  :<|> (Tagged $ \_request respond -> respond $ Wai.responseLBS HTTP.ok200 [("content-type", "fooooo")] "")
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

-- * generic client stuff

type GenericClientAPI
    = QueryParam "sqr" Int :> Get '[JSON] Int
 :<|> Capture "foo" String :> NestedAPI1

data GenericClient = GenericClient
  { getSqr          :: Maybe Int -> ClientM Int
  , mkNestedClient1 :: String -> NestedClient1
  } deriving Generic
instance SOP.Generic GenericClient
instance (Client ClientM GenericClientAPI ~ client) => ClientLike client GenericClient

type NestedAPI1
    = QueryParam "int" Int :> NestedAPI2
 :<|> QueryParam "id" Char :> Get '[JSON] Char

data NestedClient1 = NestedClient1
  { mkNestedClient2 :: Maybe Int -> NestedClient2
  , idChar          :: Maybe Char -> ClientM Char
  } deriving Generic
instance SOP.Generic NestedClient1
instance (Client ClientM NestedAPI1 ~ client) => ClientLike client NestedClient1

type NestedAPI2
    = "sum"  :> Capture "first" Int :> Capture "second" Int :> Get '[JSON] Int
 :<|> "void" :> Post '[JSON] ()

data NestedClient2 = NestedClient2
  { getSum    :: Int -> Int -> ClientM Int
  , doNothing :: ClientM ()
  } deriving Generic
instance SOP.Generic NestedClient2
instance (Client ClientM NestedAPI2 ~ client) => ClientLike client NestedClient2

genericClientServer :: Application
genericClientServer = serve (Proxy :: Proxy GenericClientAPI) (
       (\ mx -> case mx of
                  Just x -> return (x*x)
                  Nothing -> throwError $ ServantErr 400 "missing parameter" "" []
       )
  :<|> nestedServer1
 )
  where
    nestedServer1 _str = nestedServer2 :<|> (maybe (throwError $ ServantErr 400 "missing parameter" "" []) return)
    nestedServer2 _int = (\ x y -> return (x + y)) :<|> return ()

{-# NOINLINE manager' #-}
manager' :: C.Manager
manager' = unsafePerformIO $ C.newManager C.defaultManagerSettings

runClient :: ClientM a -> BaseUrl -> IO (Either ServantError a)
runClient x baseUrl' = runClientM x (mkClientEnv manager' baseUrl')

-- * utils

startWaiApp :: Application -> IO (ThreadId, BaseUrl)
startWaiApp app = do
    (port, socket) <- openTestSocket
    let settings = setPort port $ defaultSettings
    thread <- forkIO $ runSettingsSocket settings socket app
    return (thread, BaseUrl Http "localhost" port "")


endWaiApp :: (ThreadId, BaseUrl) -> IO ()
endWaiApp (thread, _) = killThread thread

openTestSocket :: IO (Port, Socket)
openTestSocket = do
  s <- socket AF_INET Stream defaultProtocol
  localhost <- inet_addr "127.0.0.1"
  bind s (SockAddrInet aNY_PORT localhost)
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
