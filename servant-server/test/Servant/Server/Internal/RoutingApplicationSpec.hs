{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Servant.Server.Internal.RoutingApplicationSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Control.Exception            hiding
                 (Handler)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
                 (register)
import           Data.IORef
import           Data.Proxy
import           GHC.TypeLits
                 (KnownSymbol, Symbol, symbolVal)
import           Network.Wai
                 (defaultRequest)
import           Servant
import           Servant.Server.Internal
import           Test.Hspec
import           Test.Hspec.Wai
                 (request, shouldRespondWith, with)

import qualified Data.Text                    as T

import           System.IO.Unsafe
                 (unsafePerformIO)

data TestResource x
    = TestResourceNone
    | TestResource x
    | TestResourceFreed
    | TestResourceError
  deriving (Eq, Show)

-- Let's not write to the filesystem
delayedTestRef :: IORef (TestResource String)
delayedTestRef = unsafePerformIO $ newIORef TestResourceNone

fromTestResource :: a -> (b -> a) -> TestResource b -> a
fromTestResource _ f (TestResource x) = f x
fromTestResource x _ _                = x

initTestResource :: IO ()
initTestResource = writeIORef delayedTestRef TestResourceNone

writeTestResource :: String -> IO ()
writeTestResource x = modifyIORef delayedTestRef $ \r -> case r of
    TestResourceNone -> TestResource x
    _                -> TestResourceError

freeTestResource :: IO ()
freeTestResource = modifyIORef delayedTestRef $ \r -> case r of
    TestResource _ -> TestResourceFreed
    _              -> TestResourceError

delayed :: DelayedIO () -> RouteResult (Handler ()) -> Delayed () (Handler ())
delayed body srv = Delayed
  { capturesD = \() -> return ()
  , methodD   = return ()
  , authD     = return ()
  , acceptD   = return ()
  , contentD  = return ()
  , paramsD   = return ()
  , headersD   = return ()
  , bodyD     = \() -> do
      liftIO (writeTestResource "hia" >> putStrLn "garbage created")
      _ <- register (freeTestResource >> putStrLn "garbage collected")
      body
  , serverD   = \() () () () _body _req -> srv
  }

simpleRun :: Delayed () (Handler ())
          -> IO ()
simpleRun d = fmap (either ignoreE id) . try $
  runAction d () defaultRequest (\_ -> return ()) (\_ -> FailFatal err500)

  where ignoreE :: SomeException -> ()
        ignoreE = const ()

-------------------------------------------------------------------------------
-- Combinator example
-------------------------------------------------------------------------------

-- | This data types writes 'sym' to 'delayedTestRef'.
data Res (sym :: Symbol)

instance (KnownSymbol sym, HasServer api ctx) => HasServer (Res sym :> api) ctx where
    type ServerT (Res sym :> api) m = IORef (TestResource String) -> ServerT api m

    hoistServerWithContext _ nc nt s = hoistServerWithContext (Proxy :: Proxy api) nc nt . s

    route Proxy ctx server = route (Proxy :: Proxy api) ctx $
        addBodyCheck server (return ()) check
      where
        sym  = symbolVal (Proxy :: Proxy sym)
        check () = do
            liftIO $ writeTestResource sym
            _ <- register freeTestResource
            return delayedTestRef

type ResApi = "foobar" :> Res "foobar" :> Get '[PlainText] T.Text

resApi :: Proxy ResApi
resApi = Proxy

resServer :: Server ResApi
resServer ref = liftIO $ fmap (fromTestResource "<wrong>" T.pack)  $ readIORef ref

-------------------------------------------------------------------------------
-- Spec
-------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Delayed" $ do
    it "actually runs clean up actions" $ do
      liftIO initTestResource
      _ <- simpleRun $ delayed (return ()) (Route $ return ())
      res <- readIORef delayedTestRef
      res `shouldBe` TestResourceFreed
    it "even with exceptions in serverD" $ do
      liftIO initTestResource
      _ <- simpleRun $ delayed (return ()) (Route $ throw DivideByZero)
      res <- readIORef delayedTestRef
      res `shouldBe` TestResourceFreed
    it "even with routing failure in bodyD" $ do
      liftIO initTestResource
      _ <- simpleRun $ delayed (delayedFailFatal err500) (Route $ return ())
      res <- readIORef delayedTestRef
      res `shouldBe` TestResourceFreed
    it "even with exceptions in bodyD" $ do
      liftIO initTestResource
      _ <- simpleRun $ delayed (liftIO $ throwIO DivideByZero) (Route $ return ())
      res <- readIORef delayedTestRef
      res `shouldBe` TestResourceFreed
  describe "ResApi" $
    with (return $ serve resApi resServer) $ do
      it "writes and cleanups resources" $ do
        liftIO initTestResource
        request "GET" "foobar" [] "" `shouldRespondWith` "foobar"
        liftIO $ do
          res <- readIORef delayedTestRef
          res `shouldBe` TestResourceFreed
