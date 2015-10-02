{-#LANGUAGE DataKinds, TypeOperators, OverloadedStrings #-}
module Main where

import Data.Text
import Data.ByteString (ByteString)
import Data.Proxy
import Servant.API
import Servant.API.Authentication
import Servant.Server
import Servant.Server.Internal.Authentication
import Network.Wai.Handler.Warp


data User = User
  { username :: ByteString
  , ha1      :: ByteString -- ^ MD5(username:realm:password)
  }


-- |  A table with just one user. namely   "username:foo:password"
table = [("username", User "username" "22f93fe54b9edf660a2f85310adc8a56")]

type API = AuthProtect (DigestAuth "foo") User 'Strict :> "foo" :> Get '[JSON] Text

api :: Server API
api = digestAuthStrict ha1 lookupUser (const . return $ "hello")

lookupUser :: DigestAuth "foo" -> IO (Maybe User)
lookupUser authData = return . lookup (daUsername authData) $ table


main :: IO ()
main = run 8080 (serve (Proxy :: Proxy API) api)
