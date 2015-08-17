{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module T5 where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Servant
import           System.Directory

type IOAPI = "myfile.txt" :> Get '[JSON] FileContent

ioAPI :: Proxy IOAPI
ioAPI = Proxy

newtype FileContent = FileContent
  { content :: String }
  deriving Generic

instance ToJSON FileContent

server :: Server IOAPI
server = do
  exists <- liftIO (doesFileExist "myfile.txt")
  if exists
    then liftIO (readFile "myfile.txt") >>= return . FileContent
    else left custom404Err

  where custom404Err = err404 { errBody = "myfile.txt just isn't there, please leave this server alone." }

app :: Application
app = serve ioAPI server
