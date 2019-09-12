{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | This module exports 'ToSourceIO' and 'FromSourceIO' for 'Proxy' and 'SafeT' instances.
module Servant.Pipes (
    PipesToSourceIO (..),
    ) where

import           Control.Monad.IO.Class
                 (MonadIO (..))
import           Control.Monad.Trans.Control
                 (liftBaseWith)
import           Pipes
                 (ListT (..))
import           Pipes.Internal
                 (Proxy (..), X, closed)
import           Pipes.Safe
                 (SafeT, runSafeT)
import           Servant.API.Stream
import qualified Servant.Types.SourceT       as S

-- | Helper class to implement @'ToSourceIO' 'Proxy'@ instance
-- for various monads.
class PipesToSourceIO m where
    pipesToSourceIO :: Proxy X () () b m () -> SourceIO b

instance PipesToSourceIO IO where
    pipesToSourceIO ma = S.SourceT ($ go ma) where
        go :: Proxy X () () b IO () -> S.StepT IO b
        go (Pure ())     = S.Stop
        go (M p)         = S.Effect (fmap go p)
        go (Request v _) = closed v
        go (Respond b n) = S.Yield b (go (n ()))

instance m ~ IO => PipesToSourceIO (SafeT m) where
    pipesToSourceIO ma =
        S.SourceT $ \k ->
        runSafeT $ liftBaseWith $ \runSafe ->
        k (go runSafe ma)
      where
        go :: (forall x. SafeT m x ->  m x)
           -> Proxy X () () b (SafeT m) ()
           -> S.StepT IO b
        go _        (Pure ())    = S.Stop
        go runSafe (M p)         = S.Effect $ runSafe $ fmap (go runSafe) p
        go _       (Request v _) = closed v
        go runSafe (Respond b n) = S.Yield b (go runSafe (n ()))

instance (PipesToSourceIO m, a' ~ X, a ~ (), b' ~ (), r ~ ())
    => ToSourceIO b (Proxy a' a b' b m r)
  where
    toSourceIO = pipesToSourceIO

instance PipesToSourceIO m => ToSourceIO a (ListT m a) where
    toSourceIO = pipesToSourceIO . enumerate

instance (MonadIO m, a' ~ X, a ~ (), b' ~ (), r ~ ())
    => FromSourceIO b (Proxy a' a b' b m r)
  where
    fromSourceIO src = M $ liftIO $ S.unSourceT src (return . go) where
        go :: S.StepT IO b -> Proxy X () () b m ()
        go S.Stop        = Pure ()
        go (S.Error err) = M (liftIO (fail err))
        go (S.Skip s)    = go s -- drives
        go (S.Effect ms) = M (liftIO (fmap go ms))
        go (S.Yield x s) = Respond x (const (go s))
    {-# SPECIALIZE INLINE fromSourceIO :: SourceIO x -> Proxy X () () x IO () #-}

instance MonadIO m => FromSourceIO a (ListT m a) where
    fromSourceIO = Select . fromSourceIO
