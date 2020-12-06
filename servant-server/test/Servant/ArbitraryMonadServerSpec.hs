{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Servant.ArbitraryMonadServerSpec where

import           Control.Monad.Reader
import           Data.Functor.Identity
import           Data.Proxy
import           Servant.API
import           Servant.Server

import           Test.Hspec
                 (Spec, describe, it)
import           Test.Hspec.Wai
                 (get, matchStatus, post, shouldRespondWith, with)

spec :: Spec
spec = describe "Arbitrary monad server" $ do
    enterSpec

type ReaderAPI = "int" :> Get '[JSON] Int
            :<|> "string" :> Post '[JSON] String

type IdentityAPI = "bool" :> Get '[JSON] Bool

type CombinedAPI = ReaderAPI :<|> IdentityAPI

readerAPI :: Proxy ReaderAPI
readerAPI = Proxy

identityAPI :: Proxy IdentityAPI
identityAPI = Proxy

combinedAPI :: Proxy CombinedAPI
combinedAPI = Proxy

readerServer' :: ServerT ReaderAPI (Reader String)
readerServer' = return 1797 :<|> ask

fReader :: Reader String a -> Handler a
fReader x = return (runReader x "hi")

readerServer :: Server ReaderAPI
readerServer = hoistServer readerAPI fReader readerServer'

combinedReaderServer' :: ServerT CombinedAPI (Reader String)
combinedReaderServer' = readerServer' :<|> hoistServer identityAPI (return . runIdentity) (return True)

combinedReaderServer :: Server CombinedAPI
combinedReaderServer = hoistServer combinedAPI fReader combinedReaderServer'

enterSpec :: Spec
enterSpec = describe "Enter" $ do
  with (return (serve readerAPI readerServer)) $ do

    it "allows running arbitrary monads" $ do
      get "int" `shouldRespondWith` "1797"
      post "string" "3" `shouldRespondWith` "\"hi\""{ matchStatus = 200 }

  with (return (serve combinedAPI combinedReaderServer)) $ do
    it "allows combnation of enters" $ do
      get "bool" `shouldRespondWith` "true"
