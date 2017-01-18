module Servant.Server.Internal.RoutingApplicationSpec (spec) where

import Prelude ()
import Prelude.Compat

import Control.Exception hiding (Handler)
import Control.Monad.Trans.Resource (register)
import Control.Monad.IO.Class
import Data.Maybe (isJust)
import Data.IORef
import Servant.Server
import Servant.Server.Internal.RoutingApplication
import Test.Hspec

import System.IO.Unsafe (unsafePerformIO)

-- Let's not write to the filesystem
delayedTestRef :: IORef (Maybe String)
delayedTestRef = unsafePerformIO $ newIORef Nothing

delayed :: DelayedIO () -> RouteResult (Handler ()) -> Delayed () (Handler ())
delayed body srv = Delayed
  { capturesD = \_ -> return ()
  , methodD   = return ()
  , authD     = return ()
  , bodyD     = do
      liftIO (writeIORef delayedTestRef (Just "hia") >> putStrLn "garbage created")
      _ <- register (writeIORef delayedTestRef Nothing >> putStrLn "garbage collected")
      body
  , serverD   = \() () _body _req -> srv
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
      cleanUpDone <- isJust <$> readIORef delayedTestRef
      cleanUpDone `shouldBe` False
    it "even with exceptions in serverD" $ do
      _ <- simpleRun $ delayed (return ()) (Route $ throw DivideByZero)
      cleanUpDone <- isJust <$> readIORef delayedTestRef
      cleanUpDone `shouldBe` False
    it "even with routing failure in bodyD" $ do
      _ <- simpleRun $ delayed (delayedFailFatal err500) (Route $ return ())
      cleanUpDone <- isJust <$> readIORef delayedTestRef
      cleanUpDone `shouldBe` False
    it "even with exceptions in bodyD" $ do
      _ <- simpleRun $ delayed (liftIO $ throwIO DivideByZero) (Route $ return ())
      cleanUpDone <- isJust <$> readIORef delayedTestRef
      cleanUpDone `shouldBe` False
