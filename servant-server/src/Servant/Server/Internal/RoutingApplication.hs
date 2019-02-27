module Servant.Server.Internal.RoutingApplication where

import           Network.Wai
                 (Application, Request, Response, ResponseReceived)
import           Prelude ()
import           Prelude.Compat
import           Servant.Server.Internal.RouteResult
import           Servant.Server.Internal.ServerError

type RoutingApplication =
     Request -- ^ the request, the field 'pathInfo' may be modified by url routing
  -> (RouteResult Response -> IO ResponseReceived) -> IO ResponseReceived

toApplication :: RoutingApplication -> Application
toApplication ra request respond = ra request routingRespond
 where
  routingRespond :: RouteResult Response -> IO ResponseReceived
  routingRespond (Fail err)      = respond $ responseServerError err
  routingRespond (FailFatal err) = respond $ responseServerError err
  routingRespond (Route v)       = respond v
