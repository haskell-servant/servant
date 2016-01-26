{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import           Data.Aeson                   (ToJSON)
import           Data.Proxy                   (Proxy (Proxy))
import           Data.Text                    (Text)
import           GHC.Generics                 (Generic)
import           Network.Wai.Handler.Warp     (run)
import           Servant.API                  ((:<|>) ((:<|>)), (:>), BasicAuth,
                                               Get, JSON)
import           Servant.API.Auth             (BasicAuthData(BasicAuthData))
import           Servant.Server               (AuthReturnType, BasicAuthResult (Authorized, Unauthorized), Config ((:.), EmptyConfig),
                                               Server, serve, BasicAuthCheck(BasicAuthCheck))

-- | let's define some types that our API returns.

-- | private data that needs protection
newtype PrivateData = PrivateData { ssshhh :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PrivateData

-- | public data that anyone can use.
newtype PublicData = PublicData { somedata :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PublicData

-- | A user we'll grab from the database when we authenticate someone
newtype User = User { userName :: Text }
  deriving (Eq, Show)

-- | a type to wrap our public api
type PublicAPI = Get '[JSON] [PublicData]

-- | a type to wrap our private api
type PrivateAPI = Get '[JSON] PrivateData

-- | our API
type API = "public"  :> PublicAPI
      :<|> "private" :> BasicAuth "foo-realm" :> PrivateAPI

-- | a value holding a proxy of our API type
api :: Proxy API
api = Proxy

-- | a value holding a proxy of our basic auth realm.
authRealm :: Proxy "foo-realm"
authRealm = Proxy

-- | Specify the data type returned after performing basic authentication
type instance AuthReturnType (BasicAuth "foo-realm") = User

-- | 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
authCheck :: BasicAuthCheck User
authCheck =
  let check (BasicAuthData username password) =
        if username == "servant" && password == "server"
        then return (Authorized (User "servant"))
        else return Unauthorized
  in BasicAuthCheck check

-- | We need to supply our handlers with the right configuration. In this case,
-- Basic Authentication requires a Config Entry with the 'BasicAuthCheck' value
-- tagged with "foo-tag" This config is then supplied to 'server' and threaded 
-- to the BasicAuth HasServer handlers.
serverConfig :: Config (BasicAuthCheck User ': '[])
serverConfig = authCheck :. EmptyConfig

-- | an implementation of our server. Here is where we pass all the handlers to our endpoints.
-- In particular, for the BasicAuth protected handler, we need to supply a function
-- that takes 'User' as an argument.
server :: Server API
server =
  let publicAPIHandler = return [PublicData "foo", PublicData "bar"]
      privateAPIHandler (user :: User) = return (PrivateData (userName user))
  in publicAPIHandler :<|> privateAPIHandler

-- | hello, server!
main :: IO ()
main = run 8080 (serve api serverConfig server)

{- Sample session

$ curl -XGET localhost:8080/public
[{"somedata":"foo"},{"somedata":"bar"}

$ curl -iXGET localhost:8080/private
HTTP/1.1 401 Unauthorized
transfer-encoding: chunked
Date: Thu, 07 Jan 2016 22:36:38 GMT
Server: Warp/3.1.8
WWW-Authenticate: Basic realm="foo-realm"

$ curl -iXGET localhost:8080/private -H "Authorization: Basic c2VydmFudDpzZXJ2ZXI="
HTTP/1.1 200 OK
transfer-encoding: chunked
Date: Thu, 07 Jan 2016 22:37:58 GMT
Server: Warp/3.1.8
Content-Type: application/json

{"ssshhh":"servant"}
-}
