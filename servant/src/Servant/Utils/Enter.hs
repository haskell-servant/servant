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
module Servant.Utils.Enter {-# DEPRECATED "Use hoistServer or hoistServerWithContext from servant-server" #-} (
    module Servant.Utils.Enter,
    -- * natural-transformation re-exports
    (:~>)(..),
    ) where

import           Control.Natural
import           Control.Monad.Identity
import           Control.Monad.Morph
import           Control.Monad.Reader
import qualified Control.Monad.State.Lazy    as LState
import qualified Control.Monad.State.Strict  as SState
import qualified Control.Monad.Writer.Lazy   as LWriter
import qualified Control.Monad.Writer.Strict as SWriter
import           Data.Tagged                 (Tagged, retag)
import           Prelude                     ()
import           Prelude.Compat
import           Servant.API

-- | Helper type family to state the 'Enter' symmetry.
type family Entered m n api where
    Entered m n (a -> api)       = a -> Entered m n api
    Entered m n (m a)            = n a
    Entered m n (api1 :<|> api2) = Entered m n api1 :<|> Entered m n api2
    Entered m n (Tagged m a)     = Tagged n a

class
    ( Entered m n typ ~ ret
    , Entered n m ret ~ typ
    ) => Enter typ m n ret | typ m n ->  ret, ret m n -> typ, ret typ m -> n, ret typ n -> m
  where
    -- | Map the leafs of an API type.
    enter :: (m :~> n) -> typ -> ret

-- ** Servant combinators

instance
    ( Enter typ1 m1 n1 ret1, Enter typ2 m2 n2 ret2
    , m1 ~ m2, n1 ~ n2
    , Entered m1 n1 (typ1 :<|> typ2) ~ (ret1 :<|> ret2)
    , Entered n1 m1 (ret1 :<|> ret2) ~ (typ1 :<|> typ2)
    ) => Enter (typ1 :<|> typ2) m1 n1 (ret1 :<|> ret2)
  where
    enter e (a :<|> b) = enter e a :<|> enter e b

instance
    ( Enter typ m n ret
    , Entered m n (a -> typ) ~ (a -> ret)
    , Entered n m (a -> ret) ~ (a -> typ)
    ) => Enter (a -> typ) m n (a -> ret)
  where
    enter arg f a = enter arg (f a)

-- ** Leaf instances

instance
    ( Entered m n (Tagged m a) ~ Tagged n a
    , Entered n m (Tagged n a) ~ Tagged m a
    ) => Enter (Tagged m a) m n (Tagged n a)
  where
    enter _ = retag

instance
    ( Entered m n (m a) ~ n a
    , Entered n m (n a) ~ m a
    ) => Enter (m a) m n (n a)
  where
    enter (NT f) = f

-- | Like `lift`.
liftNat :: (Control.Monad.Morph.MonadTrans t, Monad m) => m :~> t m
liftNat = NT Control.Monad.Morph.lift

runReaderTNat :: r -> (ReaderT r m :~> m)
runReaderTNat a = NT (`runReaderT` a)

evalStateTLNat :: Monad m => s -> (LState.StateT s m :~> m)
evalStateTLNat a = NT (`LState.evalStateT` a)

evalStateTSNat :: Monad m => s -> (SState.StateT s m :~> m)
evalStateTSNat a = NT (`SState.evalStateT` a)

-- | Log the contents of `SWriter.WriterT` with the function provided as the
-- first argument, and return the value of the @WriterT@ computation
logWriterTSNat :: MonadIO m => (w -> IO ()) -> (SWriter.WriterT w m :~> m)
logWriterTSNat logger = NT $ \x -> do
    (a, w) <- SWriter.runWriterT x
    liftIO $ logger w
    return a

-- | Like `logWriterTSNat`, but for lazy @WriterT@.
logWriterTLNat :: MonadIO m => (w -> IO ()) -> (LWriter.WriterT w m :~> m)
logWriterTLNat logger = NT $ \x -> do
    (a, w) <- LWriter.runWriterT x
    liftIO $ logger w
    return a

-- | Like @mmorph@'s `hoist`.
hoistNat :: (MFunctor t, Monad m) => (m :~> n) ->  (t m :~> t n)
hoistNat (NT n) = NT $ hoist n

-- | Like @mmorph@'s `embed`.
embedNat :: (MMonad t, Monad n) => (m :~> t n) -> (t m :~> t n)
embedNat (NT n) = NT $ embed n

-- | Like @mmorph@'s `squash`.
squashNat :: (Monad m, MMonad t) => t (t m) :~> t m
squashNat = NT squash

-- | Like @mmorph@'s `generalize`.
generalizeNat :: Applicative m => Identity :~> m
generalizeNat = NT (pure . runIdentity)
