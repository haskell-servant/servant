-- | An example of a custom authentication framework that checks a Cookie for a
-- value.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API.Authentication

-- | Data we will use to test for authentication
data CookieAuth = CookieAuth { cookie :: ByteString }

-- | An ADT to index errors we may encounter when retrieving a cookie from a request
data CookieFromRequestError = CookieNotPresent
                            | CookieInvalid

-- | A 'User' datatype we get once the authentication data is tested.
type User = ByteString

-- | An ADT to index errors we may encounter when retrieving a user from the database
-- based on the cookie
data UserFromCookieError = NoUserAssociatedWithCookie
                         | MoreThanOneCookieInDatabase Int

-- | We will look up authentication data in the database and extract a User.
type DBLookup = CookieAuth -> IO (Either UserFromCookieError User)

-- | Method that tests for authentication and extracts a User type.
userFromCookie :: DBLookup
userFromCookie (CookieAuth cookie) = if cookie == "good cookie" then return (Right "one user") else return (Left NoUserAssociatedWithCookie)

-- | a handler that takes a cookie error and returns a ServantErr in IO
missingCookieHandler :: OnMissing IO ServantErr 'Strict CookieFromRequestError
missingCookieHandler =
    let handler error = case error of
            CookieNotPresent -> return $ err401 { errReasonPhrase = "No cookie" } 
            CookieInvalid    -> return (err401 { errReasonPhrase = "Invalid Cookie" })
    in StrictMissing handler

-- | a handler that takes a Cookie + extraction error and returns a IO ServantErr.
-- This handler is called when a valid authentication data (e.g. Cookie) was extracted
-- from the request, but we were not able to find a valid user in our database.
unauthenticatedHandler :: OnUnauthenticated IO ServantErr 'Strict UserFromCookieError CookieAuth
unauthenticatedHandler =
    let handler error _ = case error of
            NoUserAssociatedWithCookie ->
                return (err403 { errReasonPhrase = "you don't exist" })
            (MoreThanOneCookieInDatabase i) ->
                return (err403 { errReasonPhrase = "we found " <> show i <> " of you. WAT?" })
    in StrictUnauthenticated handler

-- | 'AuthData' is a typeclass that provides a method to extract authentication
-- data from a 'Request'.
instance AuthData CookieAuth CookieFromRequestError where
    authData req = case lookup "Cookie" (requestHeaders req) of
        Nothing -> Left CookieNotPresent
        (Just cookieVal) -> if B.length cookieVal > 10
                            then Left CookieInvalid
                            else Right (CookieAuth cookieVal)

-- | Some data we will return from our API that is protected
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

-- | A type alias for our auth protection
type CookieAuthProtected = AuthProtect CookieAuth User 'Strict CookieFromRequestError 'Strict UserFromCookieError

-- | Our full API as a type with authentication
type API = CookieAuthProtected :> "private" :>  PrivateAPI
      :<|> PublicAPI

api :: Proxy API
api = Proxy

server :: Server API
server = authProtect missingCookieHandler
                     unauthenticatedHandler
                     userFromCookie
                     (\_ -> return prvdata)
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
