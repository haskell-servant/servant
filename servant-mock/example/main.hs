{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
import           Data.Aeson
import           GHC.Generics
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Mock
import           Test.QuickCheck.Arbitrary

newtype User = User { username :: String }
  deriving (Eq, Show, Arbitrary, Generic)

instance ToJSON User

type API = "user" :> Get '[JSON] User

api :: Proxy API
api = Proxy

main :: IO ()
main = run 8080 (serve api $ mock api)
