{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Chat (eioServer, ServerState (..)) where

import Prelude hiding (mapM_)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), pure)
#endif
import Control.Monad.State.Class (MonadState)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=))
import Data.Foldable (mapM_)

import qualified Control.Concurrent.STM as STM
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Network.SocketIO as SocketIO


data AddUser = AddUser Text.Text

instance Aeson.FromJSON AddUser where
  parseJSON = Aeson.withText "AddUser" $ pure . AddUser


data NumConnected = NumConnected !Int

instance Aeson.ToJSON NumConnected where
  toJSON (NumConnected n) = Aeson.object [ "numUsers" .= n]


data NewMessage = NewMessage Text.Text

instance Aeson.FromJSON NewMessage where
  parseJSON = Aeson.withText "NewMessage" $ pure . NewMessage


data Said = Said Text.Text Text.Text

instance Aeson.ToJSON Said where
  toJSON (Said username message) = Aeson.object
    [ "username" .= username
    , "message" .= message
    ]

data UserName = UserName Text.Text

instance Aeson.ToJSON UserName where
  toJSON (UserName un) = Aeson.object [ "username" .= un ]


data UserJoined = UserJoined Text.Text Int

instance Aeson.ToJSON UserJoined where
  toJSON (UserJoined un n) = Aeson.object
    [ "username" .= un
    , "numUsers" .= n
    ]


--------------------------------------------------------------------------------
data ServerState = ServerState { ssNConnected :: STM.TVar Int }

--server :: ServerState -> StateT SocketIO.RoutingTable Snap.Snap ()
eioServer :: forall (m :: * -> *). (MonadState SocketIO.RoutingTable m, MonadIO m) => ServerState -> m ()
eioServer state = do
  userNameMVar <- liftIO STM.newEmptyTMVarIO
  let forUserName m = liftIO (STM.atomically (STM.tryReadTMVar userNameMVar)) >>= mapM_ m

  SocketIO.on "new message" $ \(NewMessage message) ->
    forUserName $ \userName ->
      SocketIO.broadcast "new message" (Said userName message)

  SocketIO.on "add user" $ \(AddUser userName) -> do
    n <- liftIO $ STM.atomically $ do
      n <- (+ 1) <$> STM.readTVar (ssNConnected state)
      STM.putTMVar userNameMVar userName
      STM.writeTVar (ssNConnected state) n
      return n

    SocketIO.emit "login" (NumConnected n)
    SocketIO.broadcast "user joined" (UserJoined userName n)

  SocketIO.appendDisconnectHandler $ do
    (n, mUserName) <- liftIO $ STM.atomically $ do
      n <- (+ (-1)) <$> STM.readTVar (ssNConnected state)
      mUserName <- STM.tryReadTMVar userNameMVar
      STM.writeTVar (ssNConnected state) n
      return (n, mUserName)

    case mUserName of
      Nothing -> return ()
      Just userName ->
        SocketIO.broadcast "user left" (UserJoined userName n)

  SocketIO.on "typing" $
    forUserName $ \userName ->
      SocketIO.broadcast "typing" (UserName userName)

  SocketIO.on "stop typing" $
    forUserName $ \userName ->
      SocketIO.broadcast "stop typing" (UserName userName)

