{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}


import Data.Monoid ((<>))
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative((<$>))
#endif
import Network.Wai
import Servant
import Network.EngineIO.Wai
import Network.Wai.Handler.Warp (run)


import qualified Control.Concurrent.STM as STM
import qualified Network.SocketIO as SocketIO


import Chat (eioServer, ServerState (..))


type API = "socket.io" :> Raw
    :<|> Raw


api :: Proxy API
api = Proxy


server :: WaiMonad () -> Server API
server sHandler = socketIOHandler
    :<|> serveDirectory "socket-io-chat/resources"

    where
        socketIOHandler req respond = toWaiApplication sHandler req respond


app :: WaiMonad () -> Application
app sHandler = serve api $ server sHandler

port :: Int
port = 3001


main :: IO ()
main = do
    state <- ServerState <$> STM.newTVarIO 0
    sHandler <- SocketIO.initialize waiAPI (eioServer state)
    putStrLn $ "Running on " <> show port
    run port $ app sHandler


