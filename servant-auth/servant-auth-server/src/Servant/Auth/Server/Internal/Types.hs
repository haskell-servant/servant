{-# LANGUAGE CPP #-}

module Servant.Auth.Server.Internal.Types where

import Control.Applicative
import Control.Monad (MonadPlus (..), ap)
import qualified Control.Monad.Fail as Fail
import Control.Monad.Reader
import Control.Monad.Time
import Data.Monoid (Monoid (..))
import Data.Semigroup (Semigroup (..))
import Data.Time (getCurrentTime)
import GHC.Generics (Generic)
import Network.Wai (Request)

-- | The result of an authentication attempt.
data AuthResult val
  = BadPassword
  | NoSuchUser
  | -- | Authentication succeeded.
    Authenticated val
  | -- | If an authentication procedure cannot be carried out - if for example it
    -- expects a password and username in a header that is not present -
    -- @Indefinite@ is returned. This indicates that other authentication
    -- methods should be tried.
    Indefinite
  deriving (Eq, Foldable, Functor, Generic, Ord, Read, Show, Traversable)

instance Semigroup (AuthResult val) where
  Indefinite <> y = y
  x <> _ = x

instance Monoid (AuthResult val) where
  mempty = Indefinite
  mappend = (<>)

instance Applicative AuthResult where
  pure = Authenticated
  (<*>) = ap

instance Monad AuthResult where
  Authenticated v >>= f = f v
  BadPassword >>= _ = BadPassword
  NoSuchUser >>= _ = NoSuchUser
  Indefinite >>= _ = Indefinite

instance Alternative AuthResult where
  empty = mzero
  (<|>) = mplus

instance MonadPlus AuthResult where
  mzero = mempty
  mplus = (<>)

-- | An @AuthCheck@ is the function used to decide the authentication status
-- (the 'AuthResult') of a request. Different @AuthCheck@s may be combined as a
-- Monoid or Alternative; the semantics of this is that the *first*
-- non-'Indefinite' result from left to right is used and the rest are ignored.
newtype AuthCheck val = AuthCheck
  {runAuthCheck :: Request -> IO (AuthResult val)}
  deriving (Functor, Generic)

instance Semigroup (AuthCheck val) where
  AuthCheck f <> AuthCheck g = AuthCheck $ \x -> do
    fx <- f x
    case fx of
      Indefinite -> g x
      r -> pure r

instance Monoid (AuthCheck val) where
  mempty = AuthCheck $ const $ pure mempty
  mappend = (<>)

instance Applicative AuthCheck where
  pure = AuthCheck . pure . pure . pure
  (<*>) = ap

instance Monad AuthCheck where
  AuthCheck ac >>= f = AuthCheck $ \req -> do
    aresult <- ac req
    case aresult of
      Authenticated usr -> runAuthCheck (f usr) req
      BadPassword -> pure BadPassword
      NoSuchUser -> pure NoSuchUser
      Indefinite -> pure Indefinite

#if !MIN_VERSION_base(4,13,0)
  fail = Fail.fail
#endif

instance Fail.MonadFail AuthCheck where
  fail _ = AuthCheck . const $ pure Indefinite

instance MonadReader Request AuthCheck where
  ask = AuthCheck $ \x -> pure (Authenticated x)
  local f (AuthCheck check) = AuthCheck $ \req -> check (f req)

instance MonadIO AuthCheck where
  liftIO action = AuthCheck $ const $ Authenticated <$> action

instance MonadTime AuthCheck where
  currentTime = liftIO getCurrentTime

instance Alternative AuthCheck where
  empty = mzero
  (<|>) = mplus

instance MonadPlus AuthCheck where
  mzero = mempty
  mplus = (<>)
