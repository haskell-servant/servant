module Servant.Server.Internal.RoutingApplicationSpec (spec) where

import Control.Exception hiding (Handler)
import Control.Monad.IO.Class
import Servant.Server
import Servant.Server.Internal.RoutingApplication
import System.Directory
import Test.Hspec

ok :: IO (RouteResult ())
ok = return (Route ())

delayed :: DelayedIO () -> RouteResult (Handler ()) -> Delayed () (Handler ())
delayed body srv = Delayed
  { capturesD = \() -> DelayedIO $ \_req _cl -> ok
  , methodD = DelayedIO $ \_req_ _cl -> ok
  , authD = DelayedIO $ \_req _cl -> ok
  , bodyD = do
      liftIO (writeFile "delayed.test" "hia")
      addCleanup (removeFile "delayed.test" >> putStrLn "file removed")
      body
  , serverD = \() () _body _req -> srv
  }

simpleRun :: Delayed () (Handler ())
          -> IO ()
simpleRun d = fmap (either ignoreE id) . try $
  runAction d () undefined (\_ -> return ()) (\_ -> FailFatal err500)

  where ignoreE :: SomeException -> ()
        ignoreE = const ()

spec :: Spec
spec = do
  describe "Delayed" $ do
    it "actually runs clean up actions" $ do
      _ <- simpleRun $ delayed (return ()) (Route $ return ())
      fileStillThere <- doesFileExist "delayed.test"
      fileStillThere `shouldBe` False
    it "even with exceptions in serverD" $ do
      _ <- simpleRun $ delayed (return ()) (Route $ throw DivideByZero)
      fileStillThere <- doesFileExist "delayed.test"
      fileStillThere `shouldBe` False
    it "even with routing failure in bodyD" $ do
      _ <- simpleRun $ delayed (delayedFailFatal err500) (Route $ return ())
      fileStillThere <- doesFileExist "delayed.test"
      fileStillThere `shouldBe` False
    it "even with exceptions in bodyD" $ do
      _ <- simpleRun $ delayed (liftIO $ throwIO DivideByZero) (Route $ return ())
      fileStillThere <- doesFileExist "delayed.test"
      fileStillThere `shouldBe` False
