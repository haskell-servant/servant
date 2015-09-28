{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Aeson
import           Data.Monoid
import           Data.Proxy
import           Data.Text                  (Text)
import           GHC.Generics
import           Network.HTTP.Client        (Manager, defaultManagerSettings,
                                             newManager)
import           System.IO.Unsafe           (unsafePerformIO)
import           Servant.API
import           Servant.Client

import qualified Data.Text                  as T
import qualified Data.Text.IO               as T

type HackageAPI =
       "users" :> Get '[JSON] [UserSummary]
  :<|> "user" :> Capture "username" Username :> Get '[JSON] UserDetailed
  :<|> "packages" :> Get '[JSON] [Package]

type Username = Text

data UserSummary = UserSummary
  { summaryUsername :: Username
  , summaryUserid   :: Int
  } deriving (Eq, Show)

instance FromJSON UserSummary where
  parseJSON (Object o) =
    UserSummary <$> o .: "username"
                <*> o .: "userid"

  parseJSON _ = mzero

type Group = Text

data UserDetailed = UserDetailed
  { username :: Username
  , userid   :: Int
  , groups   :: [Group]
  } deriving (Eq, Show, Generic)

instance FromJSON UserDetailed

newtype Package = Package { packageName :: Text }
  deriving (Eq, Show, Generic)

instance FromJSON Package

hackageAPI :: Proxy HackageAPI
hackageAPI = Proxy


{-# NOINLINE manager #-}
manager :: Manager
manager = unsafePerformIO $ newManager defaultManagerSettings

getUsers :: ExceptT ServantError IO [UserSummary]
getUser :: Username -> ExceptT ServantError IO UserDetailed
getPackages :: ExceptT ServantError IO [Package]
getUsers :<|> getUser :<|> getPackages =
    client hackageAPI (BaseUrl Http "hackage.haskell.org" 80 "") manager

main :: IO ()
main = print =<< uselessNumbers

uselessNumbers :: IO (Either ServantError ())
uselessNumbers = runExceptT $ do
  users <- getUsers
  liftIO . putStrLn $ show (length users) ++ " users"

  user <- liftIO $ do
    putStrLn "Enter a valid hackage username"
    T.getLine
  userDetailed <- getUser user
  liftIO . T.putStrLn $ user <> " maintains " <> T.pack (show (length $ groups userDetailed)) <> " packages"

  packages <- getPackages
  let monadPackages = filter (isMonadPackage . packageName) packages
  liftIO . putStrLn $ show (length monadPackages) ++ " monad packages"

  where isMonadPackage = T.isInfixOf "monad"
