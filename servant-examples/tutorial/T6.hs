{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
module T6 where

import           Network.Wai
import           Servant

type API = "code" :> Raw IO Application

api :: Proxy API
api = Proxy

server :: Server API
server = serveDirectory "tutorial"

app :: Application
app = serve api server
