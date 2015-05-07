{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module GS8 where

import Control.Monad.Trans.Either
import Data.Aeson
import Servant
import Servant.Client

import GS3

position :: Int -- ^ value for "x"
         -> Int -- ^ value for "y"
         -> BaseUrl
         -> EitherT ServantError IO Position

hello :: Maybe String -- ^ an optional value for "name"
      -> BaseUrl
      -> EitherT ServantError IO HelloMessage

marketing :: ClientInfo -- ^ value for the request body
          -> BaseUrl
          -> EitherT ServantError IO Email

position :<|> hello :<|> marketing = client api 

baseUrl :: BaseUrl
baseUrl = BaseUrl Http "localhost" 8081

queries :: EitherT ServantError IO (Position, HelloMessage, Email)
queries = do
  pos <- position 10 10 baseUrl
  msg <- hello (Just "servant") baseUrl
  em  <- marketing (ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"]) baseUrl
  return (pos, msg, em)

run :: IO ()
run = do
  res <- runEitherT queries
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (pos, msg, em) -> do
      print pos
      print msg
      print em
