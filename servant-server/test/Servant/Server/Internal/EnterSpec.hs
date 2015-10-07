{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Servant.Server.Internal.EnterSpec where

import qualified Control.Category           as C
import           Control.Monad.Reader
import           Control.Monad.Trans.Except
import           Control.Monad.Writer
import           Data.IORef
import           Data.Proxy
import           Servant.API
import           Servant.Server

import           Control.Exception          (bracket)
import           Network.Wai                (Application)
import           Network.HTTP.Types         (methodPost)
import           Servant.Utils.StaticFiles  (serveDirectory)
import           System.Directory           (createDirectory,
                                             getCurrentDirectory,
                                             setCurrentDirectory)
import           System.IO.Temp             (withSystemTempDirectory)
import           System.IO.Unsafe           (unsafePerformIO)
import           Test.Hspec                 (Spec, around_, context, describe,
                                             it, shouldReturn)
import           Test.Hspec.Wai             (get, matchStatus, post, request,
                                             shouldRespondWith, with)

spec :: Spec
spec = describe "module Servant.Server.Enter" $ do
    enterSpec

type ReaderAPI = "int" :> Get '[JSON] Int
            :<|> "string" :> Post '[JSON] String
            :<|> "static" :> Raw (Reader String) Application

type IdentityAPI = "bool" :> Get '[JSON] Bool

type WriterAPI = "fn" :> ReqBody '[JSON] Int :> Post '[JSON] Int

type CombinedAPI = ReaderAPI :<|> IdentityAPI

type CombinedAPI2 = CombinedAPI :<|> WriterAPI

readerAPI :: Proxy ReaderAPI
readerAPI = Proxy

combinedAPI :: Proxy CombinedAPI
combinedAPI = Proxy

combinedAPI2 :: Proxy CombinedAPI2
combinedAPI2 = Proxy

readerServer' :: ServerT ReaderAPI (Reader String)
readerServer' = return 1797
    :<|> ask
    :<|> serveDirectory "static"

writerServer :: ServerT WriterAPI (WriterT String IO)
writerServer x = tell "hi" >> return x

fReader :: Reader String :~> ExceptT ServantErr IO
fReader = generalizeNat C.. runReaderTNat "hi"

readerServer :: Server ReaderAPI
readerServer = enter fReader readerServer'

combinedReaderServer' :: ServerT CombinedAPI (Reader String)
combinedReaderServer' = readerServer' :<|> enter generalizeNat (return True)

combinedReaderServer :: Server CombinedAPI
combinedReaderServer = enter fReader combinedReaderServer'

combinedServer2 :: IORef String -> Server CombinedAPI2
combinedServer2 ref'
    = enter fReader combinedReaderServer'
 :<|> enter (liftNat C.. logWriterTLNat (writeIORef ref')) writerServer

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
    it "allows combination of enters" $ do
      get "bool" `shouldRespondWith` "true"

  with (newIORef "h" >>= \r -> return (serve combinedAPI2 $ combinedServer2 r)) $ do
    it "allows nested combination of enters" $ do
      get "bool" `shouldRespondWith` "true"
      request methodPost "fn" [("Content-Type", "application/json")] "3"
        `shouldRespondWith` "3"{ matchStatus = 201 }


  context "logWriter" $ do
    with (return (serve combinedAPI2 $ combinedServer2 ref)) $ do
      it "runs the function provided with the logs as argument" $ do
        void $ request methodPost "fn" [("Content-Type", "application/json")] "3"
        liftIO $ readIORef ref `shouldReturn` "hi"


{-# NOINLINE ref #-}
ref :: IORef String
ref = unsafePerformIO $ newIORef ""
