{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
module Servant.Server.Internal.Handler where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Base
                 (MonadBase (..))
import           Control.Monad.Catch
                 (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Error.Class
                 (MonadError)
import           Control.Monad.IO.Class
                 (MonadIO)
import           Control.Monad.Trans.Control
                 (MonadBaseControl (..))
import           Control.Monad.Trans.Except
                 (ExceptT, runExceptT)
import           GHC.Generics
                 (Generic)
import           Servant.Server.Internal.ServerError
                 (ServerError)

newtype Handler a = Handler { runHandler' :: ExceptT ServerError IO a }
  deriving
    ( Functor, Applicative, Monad, MonadIO, Generic
    , MonadError ServerError
    , MonadThrow, MonadCatch, MonadMask
    )

instance MonadBase IO Handler where
  liftBase = Handler . liftBase

instance MonadBaseControl IO Handler where
  type StM Handler a = Either ServerError a

  -- liftBaseWith :: (RunInBase Handler IO -> IO a) -> Handler a
  liftBaseWith f = Handler (liftBaseWith (\g -> f (g . runHandler')))

  -- restoreM :: StM Handler a -> Handler a
  restoreM st = Handler (restoreM st)

runHandler :: Handler a -> IO (Either ServerError a)
runHandler = runExceptT . runHandler'
