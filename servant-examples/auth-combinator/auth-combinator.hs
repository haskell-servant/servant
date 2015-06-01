{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Server.Internal

-- Pretty much stolen/adapted from
-- https://github.com/haskell-servant/HaskellSGMeetup2015/blob/master/examples/authentication-combinator/AuthenticationCombinator.hs

type DBLookup = ByteString -> IO Bool

isGoodCookie :: DBLookup
isGoodCookie = return . (== "good password")

data AuthProtected

instance HasServer rest => HasServer (AuthProtected :> rest) where
  type ServerT (AuthProtected :> rest) m = ServerT rest m

  route Proxy a = WithRequest $ \ request ->
    route (Proxy :: Proxy rest) $ do
      case lookup "Cookie" (requestHeaders request) of
        Nothing -> return $ failWith $ HttpError status401 (Just "Missing auth header.")
        Just v  -> do
          authGranted <- isGoodCookie v
          if authGranted
            then a
            else return $ failWith $ HttpError status403 (Just "Invalid cookie.")

type PrivateAPI = Get '[JSON] [PrivateData]

type PublicAPI = Get '[JSON] [PublicData]

type API = "private" :> AuthProtected :> PrivateAPI
      :<|> PublicAPI

newtype PrivateData = PrivateData { ssshhh :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PrivateData

newtype PublicData = PublicData { somedata :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PublicData

api :: Proxy API
api = Proxy

server :: Server API
server = return prvdata :<|> return pubdata

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
