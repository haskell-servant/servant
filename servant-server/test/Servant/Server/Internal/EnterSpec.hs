{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Servant.Server.Internal.EnterSpec where

import qualified Control.Category           as C
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Data.Proxy
import           Servant.API
import           Servant.Server

import           Servant.Utils.StaticFiles  (serveDirectory)
import           Network.Wai                (Application)
import           Control.Exception          (bracket)
import           System.Directory           (getCurrentDirectory, setCurrentDirectory,
                                            createDirectory)
import           System.IO.Temp             (withSystemTempDirectory)
import           Test.Hspec                 (Spec, describe, it, around_)
import           Test.Hspec.Wai             (get, matchStatus, post,
                                             shouldRespondWith, with)

spec :: Spec
spec = describe "module Servant.Server.Enter" $ do
    enterSpec

type ReaderAPI' = "ep1" :> Get '[JSON] String :<|> "ep2" :> Get '[JSON] String
readerServera' :: Reader String String :<|> Reader String String
readerServera' = ask :<|> ask

x :: Reader String :~> ExceptT ServantErr IO
x = (generalizeNat C.. (runReaderTNat "hi"))
mainServer' :: Server ReaderAPI'
mainServer' = enter x readerServera'

type ReaderAPI = "int" :> Get '[JSON] Int
            :<|> "string" :> Post '[JSON] String
            :<|> "static" :> Raw (Reader String) Application

type IdentityAPI = "bool" :> Get '[JSON] Bool

type CombinedAPI = ReaderAPI :<|> IdentityAPI

readerAPI :: Proxy ReaderAPI
readerAPI = Proxy

combinedAPI :: Proxy CombinedAPI
combinedAPI = Proxy

readerServer' :: ServerT ReaderAPI (Reader String)
readerServer' = return 1797
    :<|> ask
    :<|> serveDirectory "static"

fReader :: Reader String :~> ExceptT ServantErr IO
fReader = generalizeNat C.. (runReaderTNat "hi")

readerServer :: Server ReaderAPI
readerServer = enter fReader readerServer'

combinedReaderServer' :: ServerT CombinedAPI (Reader String)
combinedReaderServer' = readerServer' :<|> enter generalizeNat (return True)

combinedReaderServer :: Server CombinedAPI
combinedReaderServer = enter fReader combinedReaderServer'

withStaticFiles :: IO () -> IO ()
withStaticFiles action = withSystemTempDirectory "servant-test" $ \ tmpDir ->
  bracket (setup tmpDir) teardown (const action)
 where
  setup tmpDir = do
    outer <- getCurrentDirectory
    setCurrentDirectory tmpDir
    createDirectory "static"
    writeFile "static/foo.txt" "bar"
    writeFile "static/index.html" "index"
    return outer

  teardown outer = do
    setCurrentDirectory outer

enterSpec :: Spec
enterSpec = describe "Enter" $ do
  around_ withStaticFiles $ with (return (serve readerAPI readerServer)) $ do

    it "allows running arbitrary monads" $ do
      get "int" `shouldRespondWith` "1797"
      post "string" "3" `shouldRespondWith` "\"hi\""{ matchStatus = 201 }

  with (return (serve combinedAPI combinedReaderServer)) $ do
    it "allows combnation of enters" $ do
      get "bool" `shouldRespondWith` "true"
