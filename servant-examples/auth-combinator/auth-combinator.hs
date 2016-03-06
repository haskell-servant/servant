{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UndecidableInstances #-}

import           Control.Monad.Trans.Except (ExceptT, throwE)
import           Data.Aeson                 hiding ((.:))
import           Data.ByteString            (ByteString)
import           Data.Monoid                ((<>))
import           Data.Map                   (Map, fromList)
import qualified Data.Map                   as Map
import           Data.Text                  (Text)
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Server.Experimental.Auth

-- | This file contains an authenticated server using servant's generalized
-- authentication support. Our basic authentication scheme is trivial: we
-- look for a cookie named "servant-auth-cookie" and its value will contain
-- a key, which we use to lookup a User. Obviously this is an absurd example,
-- but we pick something simple and non-standard to show you how to extend
-- servant's support for authentication.

-- | A user type that we "fetch from the database" after
-- performing authentication
newtype User = User { unUser :: Text }

-- | A (pure) database mapping keys to users.
database :: Map ByteString User
database = fromList [ ("key1", User "Anne Briggs")
                    , ("key2", User "Bruce Cockburn")
                    , ("key3", User "Ghédalia Tazartès")
                    ]

-- | A method that, when given a password, will return a User.
-- This is our bespoke (and bad) authentication logic.
lookupUser :: ByteString -> ExceptT ServantErr IO User
lookupUser key = case Map.lookup key database of
  Nothing -> throwE (err403 { errBody = "Invalid Cookie" })
  Just usr -> return usr

-- | The auth handler wraps a function from Request -> ExceptT ServantErr IO User
-- we look for a Cookie and pass the value of the cookie to `lookupUser`.
authHandler :: AuthHandler Request User
authHandler =
  let handler req = case lookup "servant-auth-cookie" (requestHeaders req) of
        Nothing -> throwE (err401 { errBody = "Missing auth header" })
        Just authCookieKey -> lookupUser authCookieKey
  in mkAuthHandler handler

-- | Data types that will be returned from various api endpoints
newtype PrivateData = PrivateData { ssshhh :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PrivateData

newtype PublicData = PublicData { somedata :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PublicData

-- | Our private API that we want to be auth-protected.
type PrivateAPI = Get '[JSON] [PrivateData]

-- | Our public API that doesn't have any protection
type PublicAPI = Get '[JSON] [PublicData]

-- | Our API, with auth-protection
type API = "private" :> AuthProtect "cookie-auth" :> PrivateAPI
      :<|> "public"  :> PublicAPI

-- | A value holding our type-level API
api :: Proxy API
api = Proxy

-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "cookie-auth") = User

-- | The context that will be made available to request handlers. We supply the 
-- "cookie-auth"-tagged request handler defined above, so that the 'HasServer' instance
-- of 'AuthProtect' can extract the handler and run it on the request.
serverContext :: Context (AuthHandler Request User ': '[])
serverContext = authHandler :. EmptyContext

-- | Our API, where we provide all the author-supplied handlers for each end
-- point. Note that 'privateDataFunc' is a function that takes 'User' as an
-- argument. We dont' worry about the authentication instrumentation here,
-- that is taken care of by supplying context
server :: Server API
server = privateDataFunc :<|> return publicData

  where privateDataFunc (User name) =
          return [PrivateData ("this is a secret: " <> name)]
        publicData = [PublicData "this is a public piece of data"]

-- | run our server
main :: IO ()
main = run 8080 (serveWithContext api serverContext server)

{- Sample Session:

$ curl -XGET localhost:8080/private
Missing auth header
>>>>>>> modify auth-combinator example for gen auth
>>>>>>> 8246c1f... modify auth-combinator example for gen auth

$ curl -XGET localhost:8080/private -H "servant-auth-cookie: key3"
[{"ssshhh":"this is a secret: Ghédalia Tazartès"}]

$ curl -XGET localhost:8080/private -H "servant-auth-cookie: bad-key"
Invalid Cookie

$ curl -XGET localhost:8080/public
[{"somedata":"this is a public piece of data"}]
-}

