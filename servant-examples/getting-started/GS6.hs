{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module GS6 where

import Network.Wai
import Servant

type API = "code" :> Raw

api :: Proxy API
api = Proxy

server :: Server API
server = serveDirectory "getting-started"

app :: Application
app = serve api server
