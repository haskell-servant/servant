{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Servant.Utils.Enter where

import qualified Control.Category            as C
import           Control.Monad.Identity
import           Control.Monad.Morph
import           Control.Monad.Reader
import qualified Control.Monad.State.Lazy    as LState
import qualified Control.Monad.State.Strict  as SState
import qualified Control.Monad.Writer.Lazy   as LWriter
import qualified Control.Monad.Writer.Strict as SWriter
import           Data.Typeable
import           Prelude                     ()
import           Prelude.Compat

import           Servant.API

class Enter typ arg ret | typ arg -> ret, typ ret -> arg where
    enter :: arg -> typ -> ret

-- **  Servant combinators
instance ( Enter typ1 arg1 ret1, Enter typ2 arg2 ret2
         , arg1 ~ arg2
         ) => Enter (typ1 :<|> typ2) arg1 (ret1 :<|> ret2) where
    enter e (a :<|> b) = enter e a :<|> enter e b

instance (Enter b arg ret) => Enter (a -> b) arg (a -> ret) where
    enter arg f a = enter arg (f a)

-- ** Useful instances

-- | A natural transformation from @m@ to @n@. Used to `enter` particular
-- datatypes.
newtype m :~> n = Nat { unNat :: forall a. m a -> n a} deriving Typeable

instance C.Category (:~>) where
    id = Nat id
    Nat f . Nat g = Nat (f . g)

instance Enter (m a) (m :~> n) (n a) where
    enter (Nat f) = f

-- | Like `lift`.
liftNat :: (Control.Monad.Morph.MonadTrans t, Monad m) => m :~> t m
liftNat = Nat Control.Monad.Morph.lift

runReaderTNat :: r -> (ReaderT r m :~> m)
runReaderTNat a = Nat (`runReaderT` a)

evalStateTLNat :: Monad m => s -> (LState.StateT s m :~> m)
evalStateTLNat a = Nat (`LState.evalStateT` a)

evalStateTSNat :: Monad m => s -> (SState.StateT s m :~> m)
evalStateTSNat a = Nat (`SState.evalStateT` a)

-- | Log the contents of `SWriter.WriterT` with the function provided as the
-- first argument, and return the value of the @WriterT@ computation
logWriterTSNat :: MonadIO m => (w -> IO ()) -> (SWriter.WriterT w m :~> m)
logWriterTSNat logger = Nat $ \x -> do
    (a, w) <- SWriter.runWriterT x
    liftIO $ logger w
    return a

-- | Like `logWriterTSNat`, but for strict @WriterT@.
logWriterTLNat :: MonadIO m => (w -> IO ()) -> (LWriter.WriterT w m :~> m)
logWriterTLNat logger = Nat $ \x -> do
    (a, w) <- LWriter.runWriterT x
    liftIO $ logger w
    return a

-- | Like @mmorph@'s `hoist`.
hoistNat :: (MFunctor t, Monad m) => (m :~> n) ->  (t m :~> t n)
hoistNat (Nat n) = Nat $ hoist n

-- | Like @mmorph@'s `embed`.
embedNat :: (MMonad t, Monad n) => (m :~> t n) -> (t m :~> t n)
embedNat (Nat n) = Nat $ embed n

-- | Like @mmorph@'s `squash`.
squashNat :: (Monad m, MMonad t) => t (t m) :~> t m
squashNat = Nat squash

-- | Like @mmorph@'s `generalize`.
generalizeNat :: Applicative m => Identity :~> m
generalizeNat = Nat (pure . runIdentity)
