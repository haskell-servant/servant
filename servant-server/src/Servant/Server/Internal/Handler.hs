{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Servant.Server.Internal.Handler where

import Control.Monad.Base (MonadBase (..))
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT)
import Data.String (fromString)
import GHC.Generics (Generic)

import Servant.Server.Internal.ServerError (ServerError, err500, errBody)

newtype Handler a = Handler {runHandler' :: ExceptT ServerError IO a}
  deriving stock (Generic)
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadCatch
    , MonadError ServerError
    , MonadIO
    , MonadMask
    , MonadThrow
    )

instance MonadFail Handler where
  fail str = throwError err500{errBody = fromString str}

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

-- | Pattern synonym that matches directly on the inner 'IO' action.
--
-- To lift 'IO' actions that don't carry a 'ServerError', use 'Control.Monad.IO.Class.liftIO' instead.
pattern MkHandler :: IO (Either ServerError a) -> Handler a
pattern MkHandler ioe = Handler (ExceptT ioe)

{-# COMPLETE MkHandler #-}
