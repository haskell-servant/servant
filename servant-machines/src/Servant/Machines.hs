{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module exports 'ToSourceIO' and 'FromSourceIO' for 'MachineT' instances.
module Servant.Machines
  ( MachineToSourceIO (..)
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Machine (MachineT (..), Step (..))
import Servant.API.Stream
import qualified Servant.Types.SourceT as S

-- | Helper class to implement @'ToSourceIO' 'MachineT'@ instance
-- for various monads.
class MachineToSourceIO m where
  machineToSourceIO :: MachineT m k o -> S.SourceT IO o

instance MachineToSourceIO IO where
  machineToSourceIO ma = S.SourceT ($ go ma)
    where
      go (MachineT m) = S.Effect $ do
        step <- m
        case step of
          Stop -> pure S.Stop
          Yield x m' -> pure (S.Yield x (go m'))
          Await _ _ m' -> pure (S.Skip (go m'))

instance MachineToSourceIO m => ToSourceIO o (MachineT m k o) where
  toSourceIO = machineToSourceIO

instance MonadIO m => FromSourceIO o (MachineT m k o) where
  fromSourceIO src = pure $ MachineT $ liftIO $ S.unSourceT src go
    where
      go :: S.StepT IO o -> IO (Step k o (MachineT m k o))
      go S.Stop = pure Stop
      go (S.Error err) = fail err
      go (S.Skip s) = go s
      go (S.Effect ms) = ms >>= go
      go (S.Yield x s) = pure (Yield x (MachineT (liftIO (go s))))
  {-# SPECIALIZE INLINE fromSourceIO :: SourceIO o -> IO (MachineT IO k o) #-}
