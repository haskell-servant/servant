{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Servant.Server.Internal.RouteResult where

import           Control.Monad
                 (ap, liftM)
import           Control.Monad.Base
                 (MonadBase (..))
import           Control.Monad.Catch
                 (MonadThrow (..))
import           Control.Monad.Trans
                 (MonadIO (..), MonadTrans (..))
import           Control.Monad.Trans.Control
                 (ComposeSt, MonadBaseControl (..), MonadTransControl (..),
                 defaultLiftBaseWith, defaultRestoreM)

import           Servant.Server.Internal.ServerError

-- | The result of matching against a path in the route tree.
data RouteResult a =
    Fail ServerError           -- ^ Keep trying other paths.
                               --   The 'ServantError' should only be 404, 405 or 406.
  | FailFatal !ServerError     -- ^ Don't try other paths.
  | Route !a
  deriving (Eq, Show, Read, Functor)

instance Applicative RouteResult where
    pure = return
    (<*>) = ap

instance Monad RouteResult where
    return = Route
    Route a     >>= f = f a
    Fail e      >>= _ = Fail e
    FailFatal e >>= _ = FailFatal e

newtype RouteResultT m a = RouteResultT { runRouteResultT :: m (RouteResult a) }
  deriving (Functor)

instance MonadTrans RouteResultT where
    lift = RouteResultT . liftM Route

instance (Functor m, Monad m) => Applicative (RouteResultT m) where
    pure  = return
    (<*>) = ap

instance Monad m => Monad (RouteResultT m) where
    return = RouteResultT . return . Route
    m >>= k = RouteResultT $ do
        a <- runRouteResultT m
        case a of
            Fail e      -> return $ Fail e
            FailFatal e -> return $ FailFatal e
            Route b     -> runRouteResultT (k b)

instance MonadIO m => MonadIO (RouteResultT m) where
    liftIO = lift . liftIO

instance MonadBase b m => MonadBase b (RouteResultT m) where
    liftBase = lift . liftBase

instance MonadBaseControl b m => MonadBaseControl b (RouteResultT m) where
    type StM (RouteResultT m) a = ComposeSt RouteResultT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM

instance MonadTransControl RouteResultT where
    type StT RouteResultT a = RouteResult a
    liftWith f = RouteResultT $ liftM return $ f runRouteResultT
    restoreT = RouteResultT

instance MonadThrow m => MonadThrow (RouteResultT m) where
    throwM = lift . throwM
