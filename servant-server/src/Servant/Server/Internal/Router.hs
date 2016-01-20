{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE CPP #-}
module Servant.Server.Internal.Router where

import           Data.Map                                   (Map)
import qualified Data.Map                                   as M
import           Data.Text                                  (Text)
import           Network.Wai                                (Request, Response, pathInfo)
import           Servant.Server.Internal.RoutingApplication
import           Servant.Server.Internal.ServantErr

type Router = Router' RoutingApplication

-- | Internal representation of a router.
data Router' a =
    WithRequest   (Request -> (Request, Router))
      -- ^ current request is passed to the router and can be changed
  | StaticRouter  (Map Text Router)
      -- ^ first path component used for lookup and removed afterwards
  | DynamicRouter (Text -> Router)
      -- ^ first path component used for lookup and removed afterwards
  | LeafRouter    a
      -- ^ to be used for routes that match an empty path
  | Choice        Router Router
      -- ^ left-biased choice between two routers
  deriving Functor

-- | Apply a transformation to the response of a `Router`.
tweakResponse :: (RouteResult Response -> RouteResult Response) -> Router -> Router
tweakResponse f = fmap (\a -> \req cont -> a req (cont . f))

-- | Smart constructor for the choice between routers.
-- We currently optimize the following cases:
--
--   * Two static routers can be joined by joining their maps.
--   * Two dynamic routers can be joined by joining their codomains.
--
choice :: Router -> Router -> Router
choice (StaticRouter table1) (StaticRouter table2) =
  StaticRouter (M.unionWith choice table1 table2)
choice (DynamicRouter fun1)  (DynamicRouter fun2)  =
  DynamicRouter (\ first -> choice (fun1 first) (fun2 first))
choice router1 router2 = Choice router1 router2

-- | Interpret a router as an application.
runRouter :: Router -> RoutingApplication
runRouter (WithRequest router) request respond =
  runRouter subRouter subRequest respond
  where
    (subRequest, subRouter) = router request
runRouter (StaticRouter table) request respond =
  case pathInfo request of
    first : rest
      | Just router <- M.lookup first table
      -> let request' = request { pathInfo = rest }
         in  runRouter router request' respond
    _ -> respond $ Fail err404
runRouter (DynamicRouter fun)  request respond =
  case pathInfo request of
    first : rest
      -> let request' = request { pathInfo = rest }
         in  runRouter (fun first) request' respond
    _ -> respond $ Fail err404
runRouter (LeafRouter app)     request respond = app request respond
runRouter (Choice r1 r2)       request respond =
  runRouter r1 request $ \ mResponse1 -> case mResponse1 of
    Fail _ -> runRouter r2 request $ \ mResponse2 ->
      respond (highestPri mResponse1 mResponse2)
    _      -> respond mResponse1
   where
     highestPri (Fail e1) (Fail e2) =
       if worseHTTPCode (errHTTPCode e1) (errHTTPCode e2)
         then Fail e2
         else Fail e1
     highestPri (Fail _) y = y
     highestPri x _ = x


-- Priority on HTTP codes.
--
-- It just so happens that 404 < 405 < 406 as far as
-- we are concerned here, so we can use (<).
worseHTTPCode :: Int -> Int -> Bool
worseHTTPCode = (<)
