{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
import           Data.Aeson
import           Data.ByteString          (ByteString)
import           Data.Text                (Text)
import           GHC.Generics
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Server.Internal
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Builder.Internal (byteStringCopy)
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API.Authentication
import Servant.Server.Internal
import Servant.Server.Internal.Authentication (strictProtect, AuthHandlers(AuthHandlers))

-- | An example of a custom authentication framework that checks a Cookie for a
-- value.

-- | Data we will use to test for authentication
data CookieAuth = CookieAuth { cookie :: ByteString }

-- | a 'User' datatype we get once the authentication data is tested.
type User = ByteString

-- | we will look up authentication data in the database and extract a User.
type DBLookup = CookieAuth -> IO (Maybe User)

-- | method that tests for authentication and extracts a User type.
isGoodCookie :: DBLookup
isGoodCookie (CookieAuth cookie) = if cookie == "good cookie" then return (Just "one user") else return Nothing

-- | Response handlers: what do we do when authentication doesn't work.
cookieAuthHandlers :: AuthHandlers CookieAuth
cookieAuthHandlers = AuthHandlers missingAuth notAuthenticated
  where
    missingAuth :: IO Response
    missingAuth = return $ (responseBuilder status401 [] "Missing Cookie header.")

    notAuthenticated :: CookieAuth -> IO Response
    notAuthenticated (CookieAuth cookie) = return $
        responseBuilder status401 [] ("Invalid cookie: " <> byteStringCopy cookie)

-- | 'AuthData' is a typeclass that provides a method to extract authentication
-- data from a 'Reqest'
instance AuthData CookieAuth where
    authData req = fmap CookieAuth (lookup "Cookie" (requestHeaders req))

-- | some data we will return from our API that is protected
newtype PrivateData = PrivateData { ssshhh :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PrivateData

-- | Some data we will return from our API that is not protected
newtype PublicData = PublicData { somedata :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PublicData

-- | Private API (will require authentication) as a type
type PrivateAPI = Get '[JSON] [PrivateData]

-- | Public API (non-authenticated) as a type
type PublicAPI = Get '[JSON] [PublicData]

-- | Our full API as a type with authentication
type API = AuthProtect CookieAuth User 'Strict :> "private" :>  PrivateAPI
      :<|> PublicAPI

api :: Proxy API
api = Proxy

server :: Server API
server = strictProtect isGoodCookie (const (return prvdata)) cookieAuthHandlers
    :<|> return pubdata

  where prvdata = [PrivateData "this is a secret"]
        pubdata = [PublicData "this is a public piece of data"]

main :: IO ()
main = run 8080 (serve api server)

{- Sample session:
$ curl http://localhost:8080/
[{"somedata":"this is a public piece of data"}]
$ curl http://localhost:8080/private
Missing auth header.
$ curl -H "Cookie: good password" http://localhost:8080/private
[{"ssshhh":"this is a secret"}]
$ curl -H "Cookie: bad password" http://localhost:8080/private
Invalid cookie.
-}
