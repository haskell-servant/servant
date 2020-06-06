{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
module Servant.Server.Internal.DelayedIO where

import           Control.Monad.Base
                 (MonadBase (..))
import           Control.Monad.Catch
                 (MonadThrow (..))
import           Control.Monad.Reader
                 (MonadReader (..), ReaderT (..), runReaderT)
import           Control.Monad.Trans
                 (MonadIO (..), MonadTrans (..))
import           Control.Monad.Trans.Control
                 (MonadBaseControl (..))
import           Control.Monad.Trans.Resource
                 (MonadResource (..), ResourceT, runInternalState,
                 transResourceT, withInternalState)
import           Network.Wai
                 (Request)

import           Servant.Server.Internal.RouteResult
import           Servant.Server.Internal.ServerError

-- | Computations used in a 'Delayed' can depend on the
-- incoming 'Request', may perform 'IO', and result in a
-- 'RouteResult', meaning they can either succeed, fail
-- (with the possibility to recover), or fail fatally.
--
newtype DelayedIO a = DelayedIO { runDelayedIO' :: ReaderT Request (ResourceT (RouteResultT IO)) a }
  deriving
    ( Functor, Applicative, Monad
    , MonadIO, MonadReader Request
    , MonadThrow
    , MonadResource
    )

instance MonadBase IO DelayedIO where
    liftBase = liftIO

liftRouteResult :: RouteResult a -> DelayedIO a
liftRouteResult x = DelayedIO $ lift . lift $ RouteResultT . return $ x

instance MonadBaseControl IO DelayedIO where
    -- type StM DelayedIO a = StM (ReaderT Request (ResourceT (RouteResultT IO))) a
    -- liftBaseWith f = DelayedIO $ liftBaseWith $ \g -> f (g . runDelayedIO')
    -- restoreM       = DelayedIO . restoreM

    type StM DelayedIO a = RouteResult a
    liftBaseWith f = DelayedIO $ ReaderT $ \req -> withInternalState $ \s ->
        liftBaseWith $ \runInBase -> f $ \x ->
            runInBase (runInternalState (runReaderT (runDelayedIO' x) req) s)
    restoreM      = DelayedIO . lift . withInternalState . const . restoreM


runDelayedIO :: DelayedIO a -> Request -> ResourceT IO (RouteResult a)
runDelayedIO m req = transResourceT runRouteResultT $ runReaderT (runDelayedIO' m) req

-- | Fail with the option to recover.
delayedFail :: ServerError -> DelayedIO a
delayedFail err = liftRouteResult $ Fail err

-- | Fail fatally, i.e., without any option to recover.
delayedFailFatal :: ServerError -> DelayedIO a
delayedFailFatal err = liftRouteResult $ FailFatal err

-- | Gain access to the incoming request.
withRequest :: (Request -> DelayedIO a) -> DelayedIO a
withRequest f = do
    req <- ask
    f req
