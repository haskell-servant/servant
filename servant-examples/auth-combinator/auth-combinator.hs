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
import           Data.Text                  (Text)
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

-- | A user type that we "fetch from the database" after
-- performing authentication
newtype User = User { unUser :: Text }


-- | A method that, when given a password, will return a User.
-- This is our bespoke (and bad) authentication logic.
lookupUser :: ByteString -> ExceptT ServantErr IO User
lookupUser cookie =
  if cookie == "good password"
  then return (User "user")
  else throwE (err403 { errBody = "Invalid Cookie" })

-- | The auth handler wraps a function from Request -> ExceptT ServantErr IO User
-- we look for a Cookie and pass the value of the cookie to `lookupUser`.
authHandler :: AuthHandler Request User
authHandler =
  let handler req = case lookup "Cookie" (requestHeaders req) of
        Nothing -> throwE (err401 { errBody = "Missing auth header" })
        Just cookie -> lookupUser cookie
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
type instance AuthReturnType (AuthProtect "cookie-auth") = User

-- | The configuration that will be made available to request handlers. We supply the 
-- "cookie-auth"-tagged request handler defined above, so that the 'HasServer' instance
-- of 'AuthProtect' can extract the handler and run it on the request.
serverConfig :: Config (AuthHandler Request User ': '[])
serverConfig = authHandler :. EmptyConfig

-- | Our API, where we provide all the author-supplied handlers for each end point.
-- note that 'prvdata' is a function that takes 'User' as an argument. We dont' worry
-- about the authentication instrumentation here, that is taken care of by supplying
-- configuration
server :: Server API
server = prvdata :<|> return pubdata

  where prvdata (User name) = return [PrivateData ("this is a secret: " <> name)]
        pubdata = [PublicData "this is a public piece of data"]

-- | run our server
main :: IO ()
main = run 8080 (serve api serverConfig server)

{- Sample Session:

$ curl -XGET localhost:8080/private
Missing auth header

$ curl -XGET localhost:8080/private -H "Cookie: good password"
[{"ssshhh":"this is a secret: user"}]

$ curl -XGET localhost:8080/private -H "Cookie: bad password"
Invalid Cookie

$ curl -XGET localhost:8080/public
[{"somedata":"this is a public piece of data"}]
-}
