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

import           Data.Aeson
import           Data.ByteString          (ByteString)
import           Data.IORef
import           Data.Text                (Text)
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Server.Internal

-- Pretty much stolen/adapted from
-- https://github.com/haskell-servant/HaskellSGMeetup2015/blob/master/examples/authentication-combinator/AuthenticationCombinator.hs

type DBConnection = IORef [ByteString]
type DBLookup = DBConnection -> ByteString -> IO Bool

initDB :: IO DBConnection
initDB = newIORef ["good password"]

isGoodCookie :: DBLookup
isGoodCookie ref password = do
  allowed <- readIORef ref
  return (password `elem` allowed)

data AuthProtected

instance (HasConfigEntry config DBConnection, HasServer rest config)
  => HasServer (AuthProtected :> rest) config where

  type ServerT (AuthProtected :> rest) m = ServerT rest m

  route Proxy config subserver = WithRequest $ \ request ->
    route (Proxy :: Proxy rest) config $ addAcceptCheck subserver $ cookieCheck request
      where
        cookieCheck req = case lookup "Cookie" (requestHeaders req) of
            Nothing -> return $ FailFatal err401 { errBody = "Missing auth header" }
            Just v  -> do
              let dbConnection = getConfigEntry config
              authGranted <- isGoodCookie dbConnection v
              if authGranted
                then return $ Route ()
                else return $ FailFatal err403 { errBody = "Invalid cookie" }

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
main = do
  dbConnection <- initDB
  let config = dbConnection :. EmptyConfig
  run 8080 (serve api config server)

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
