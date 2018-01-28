{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
module Servant.Server.Internal.Handler where

import Prelude ()
import Prelude.Compat

import           Control.Monad.Base                 (MonadBase (..))
import           Control.Monad.Catch                (MonadCatch, MonadThrow)
import           Control.Monad.Error.Class          (MonadError)
import           Control.Monad.IO.Class             (MonadIO)
import           Control.Monad.Trans.Control        (MonadBaseControl (..))
import           Control.Monad.Trans.Except         (ExceptT, runExceptT)
import           GHC.Generics                       (Generic)
import           Servant.Server.Internal.ServantErr (ServantErr)

newtype Handler a = Handler { runHandler' :: ExceptT ServantErr IO a }
  deriving
    ( Functor, Applicative, Monad, MonadIO, Generic
    , MonadError ServantErr
    , MonadThrow, MonadCatch
    )

instance MonadBase IO Handler where
  liftBase = Handler . liftBase

instance MonadBaseControl IO Handler where
  type StM Handler a = Either ServantErr a

  -- liftBaseWith :: (RunInBase Handler IO -> IO a) -> Handler a
  liftBaseWith f = Handler (liftBaseWith (\g -> f (g . runHandler')))

  -- restoreM :: StM Handler a -> Handler a
  restoreM st = Handler (restoreM st)

runHandler :: Handler a -> IO (Either ServantErr a)
runHandler = runExceptT . runHandler'
