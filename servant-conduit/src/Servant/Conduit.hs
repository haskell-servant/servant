{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | This module exports 'ToSourceIO' and 'FromSourceIO' for 'ConduitT' instances.
module Servant.Conduit (
    ConduitToSourceIO (..),
    ) where

import           Control.Monad.IO.Class
                 (MonadIO (..))
import           Control.Monad.IO.Unlift
                 (MonadUnliftIO (..))
import           Control.Monad.Trans.Resource
                 (ResourceT, runResourceT)
import           Data.Conduit.Internal
                 (ConduitT (..), Pipe (..))
import           Servant.API.Stream
import qualified Servant.Types.SourceT        as S

-- | Helper class to implement @'ToSourceIO' 'ConduitT'@ instance
-- for various monads.
class ConduitToSourceIO m where
    conduitToSourceIO :: ConduitT i o m () -> SourceIO o

instance ConduitToSourceIO IO where
    conduitToSourceIO (ConduitT con) = S.SourceT ($ go (con Done)) where
        go p0 = case p0 of
            Done ()          -> S.Stop
            HaveOutput p o   -> S.Yield o (go p)
            NeedInput _ip up -> S.Skip (go (up ()))
            PipeM m          -> S.Effect $ fmap go m
            Leftover p _l    -> S.Skip (go p)

instance m ~ IO => ConduitToSourceIO (ResourceT m) where
    conduitToSourceIO (ConduitT con) =
        S.SourceT $ \k ->
        runResourceT $ withRunInIO $ \runRes ->
        k (go runRes (con Done))
      where
        go :: (forall x. ResourceT m x -> m x)
           -> Pipe i i o () (ResourceT m) ()
           -> S.StepT IO o
        go _      (Done ())          = S.Stop
        go runRes (HaveOutput p o)   = S.Yield o (go runRes p)
        go runRes (NeedInput _ip up) = S.Skip (go runRes (up ()))
        go runRes (PipeM m)          = S.Effect $ runRes $ fmap (go runRes) m
        go runRes (Leftover p _l)    = S.Skip (go runRes p)

instance (ConduitToSourceIO m, r ~ ())
    => ToSourceIO o (ConduitT i o m r)
  where
    toSourceIO = conduitToSourceIO

instance (MonadIO m, r ~ ()) => FromSourceIO o (ConduitT i o m r) where
    fromSourceIO src =
        ConduitT $ \con ->
        PipeM $ liftIO $ S.unSourceT src $ \step ->
        loop con step
      where
        loop :: MonadIO m => (() -> Pipe i i o () m b) -> S.StepT IO o -> IO (Pipe i i o () m b)
        loop  con S.Stop        = return (con ())
        loop _con (S.Error err) = fail err
        loop  con (S.Skip s)    = loop con s
        loop  con (S.Effect ms) = ms >>= loop con
        loop  con (S.Yield x s) = return (HaveOutput (PipeM (liftIO $ loop con s)) x)

    {-# SPECIALIZE INLINE fromSourceIO :: SourceIO o -> ConduitT i o IO () #-}
