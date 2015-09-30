{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
module T8 where

import           Control.Monad.Trans.Except
import           Network.HTTP.Client        (Manager, defaultManagerSettings,
                                             newManager)
import           Servant
import           Servant.Client
import           System.IO.Unsafe           (unsafePerformIO)

import           T3

position :: Int -- ^ value for "x"
         -> Int -- ^ value for "y"
         -> ExceptT ServantError IO Position

hello :: Maybe String -- ^ an optional value for "name"
      -> ExceptT ServantError IO HelloMessage

marketing :: ClientInfo -- ^ value for the request body
          -> ExceptT ServantError IO Email

position :<|> hello :<|> marketing = client api baseUrl manager

baseUrl :: BaseUrl
baseUrl = BaseUrl Http "localhost" 8081 ""

{-# NOINLINE manager #-}
manager :: Manager
manager = unsafePerformIO $ newManager defaultManagerSettings

queries :: ExceptT ServantError IO (Position, HelloMessage, Email)
queries = do
  pos <- position 10 10
  msg <- hello (Just "servant")
  em  <- marketing (ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"])
  return (pos, msg, em)

run :: IO ()
run = do
  res <- runExceptT queries
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (pos, msg, em) -> do
      print pos
      print msg
      print em
