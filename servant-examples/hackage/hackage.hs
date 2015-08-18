{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Monoid
import           Data.Proxy
import           Data.Text                  (Text)
import           GHC.Generics
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

getUsers :: EitherT ServantError IO [UserSummary]
getUser :: Username -> EitherT ServantError IO UserDetailed
getPackages :: EitherT ServantError IO [Package]
getUsers :<|> getUser :<|> getPackages = client hackageAPI $ BaseUrl Http "hackage.haskell.org" 80

main :: IO ()
main = print =<< uselessNumbers

uselessNumbers :: IO (Either ServantError ())
uselessNumbers = runEitherT $ do
  users <- getUsers
  liftIO . putStrLn $ show (length users) ++ " users"

  user <- liftIO $ do
    putStrLn "Enter a valid hackage username"
    T.getLine
  userDetailed <- (getUser user)
  liftIO . T.putStrLn $ user <> " maintains " <> T.pack (show (length $ groups userDetailed)) <> " packages"

  packages <- getPackages
  let monadPackages = filter (isMonadPackage . packageName) packages
  liftIO . putStrLn $ show (length monadPackages) ++ " monad packages"

  where isMonadPackage = T.isInfixOf "monad"
