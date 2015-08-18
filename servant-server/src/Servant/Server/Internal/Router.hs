module Servant.Server.Internal.Router where

import           Data.Map                                   (Map)
import qualified Data.Map                                   as M
import           Data.Monoid                                ((<>))
import           Data.Text                                  (Text)
import           Network.Wai                                (Request, pathInfo)
import           Servant.Server.Internal.PathInfo
import           Servant.Server.Internal.RoutingApplication

-- | Internal representation of a router.
data Router =
    WithRequest   (Request -> Router)
      -- ^ current request is passed to the router
  | StaticRouter  (Map Text Router)
      -- ^ first path component used for lookup and removed afterwards
  | DynamicRouter (Text -> Router)
      -- ^ first path component used for lookup and removed afterwards
  | LeafRouter    RoutingApplication
      -- ^ to be used for routes that match an empty path
  | Choice        Router Router
      -- ^ left-biased choice between two routers

-- | Smart constructor for the choice between routers.
-- We currently optimize the following cases:
--
--   * Two static routers can be joined by joining their maps.
--   * Two dynamic routers can be joined by joining their codomains.
--   * Two 'WithRequest' routers can be joined by passing them
--     the same request and joining their codomains.
--   * A 'WithRequest' router can be joined with anything else by
--     passing the same request to both but ignoring it in the
--     component that does not need it.
--
choice :: Router -> Router -> Router
choice (StaticRouter table1) (StaticRouter table2) =
  StaticRouter (M.unionWith choice table1 table2)
choice (DynamicRouter fun1)  (DynamicRouter fun2)  =
  DynamicRouter (\ first -> choice (fun1 first) (fun2 first))
choice (WithRequest router1) (WithRequest router2) =
  WithRequest (\ request -> choice (router1 request) (router2 request))
choice (WithRequest router1) router2 =
  WithRequest (\ request -> choice (router1 request) router2)
choice router1 (WithRequest router2) =
  WithRequest (\ request -> choice router1 (router2 request))
choice router1 router2 = Choice router1 router2

-- | Interpret a router as an application.
runRouter :: Router -> RoutingApplication
runRouter (WithRequest router) request respond =
  runRouter (router request) request respond
runRouter (StaticRouter table) request respond =
  case processedPathInfo request of
    first : rest
      | Just router <- M.lookup first table
      -> let request' = request { pathInfo = rest }
         in  runRouter router request' respond
    _ -> respond $ failWith NotFound
runRouter (DynamicRouter fun)  request respond =
  case processedPathInfo request of
    first : rest
      -> let request' = request { pathInfo = rest }
         in  runRouter (fun first) request' respond
    _ -> respond $ failWith NotFound
runRouter (LeafRouter app)     request respond = app request respond
runRouter (Choice r1 r2)       request respond =
  runRouter r1 request $ \ mResponse1 ->
    if isMismatch mResponse1
      then runRouter r2 request $ \ mResponse2 ->
             respond (mResponse1 <> mResponse2)
      else respond mResponse1

