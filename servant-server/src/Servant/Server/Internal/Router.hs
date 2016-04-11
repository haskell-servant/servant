{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Servant.Server.Internal.Router where

import           Data.Map                                   (Map)
import qualified Data.Map                                   as M
import           Data.Monoid
import           Data.Text                                  (Text)
import qualified Data.Text                                  as T
import           Network.Wai                                (Request, Response, pathInfo)
import           Servant.Server.Internal.RoutingApplication
import           Servant.Server.Internal.ServantErr

type Router = Router' RoutingApplication

-- | Internal representation of a router.
data Router' a =
    WithRequest   (Request -> Router' a)
      -- ^ current request is passed to the router
  | StaticRouter  (Map Text (Router' a)) [a]
      -- ^ the map contains routers for subpaths (first path component used
      --   for lookup and removed afterwards), the list contains handlers
      --   for the empty path, to be tried in order
  | DynamicRouter (Text -> Router' a)
      -- ^ first path component passed to the function and removed afterwards
  | RawRouter     a
      -- ^ to be used for routes we do not know anything about
  | Choice        (Router' a) (Router' a)
      -- ^ left-biased choice between two routers
  deriving Functor

-- | Smart constructor for a single static path component.
pathRouter :: Text -> Router' a -> Router' a
pathRouter t r = StaticRouter (M.singleton t r) []

-- | Smart constructor for a leaf, i.e., a router that expects
-- the empty path.
--
leafRouter :: a -> Router' a
leafRouter l = StaticRouter M.empty [l]

-- | Smart constructor for the choice between routers.
-- We currently optimize the following cases:
--
--   * Two static routers can be joined by joining their maps
--     and concatenating their leaf-lists.
--   * Two dynamic routers can be joined by joining their codomains.
--   * Two 'WithRequest' routers can be joined by passing them
--     the same request and joining their codomains.
--   * A 'WithRequest' router can be joined with anything else by
--     passing the same request to both but ignoring it in the
--     component that does not need it.
--   * Choice nodes can be reordered.
--
choice :: Router -> Router -> Router
choice (StaticRouter table1 ls1) (StaticRouter table2 ls2) =
  StaticRouter (M.unionWith choice table1 table2) (ls1 ++ ls2)
choice (DynamicRouter fun1)  (DynamicRouter fun2)  =
  DynamicRouter (\ first -> choice (fun1 first) (fun2 first))
choice (WithRequest router1) (WithRequest router2) =
  WithRequest (\ request -> choice (router1 request) (router2 request))
choice (WithRequest router1) router2 =
  WithRequest (\ request -> choice (router1 request) router2)
choice router1 (WithRequest router2) =
  WithRequest (\ request -> choice router1 (router2 request))
choice router1 (Choice router2 router3) = Choice (choice router1 router2) router3
choice router1 router2 = Choice router1 router2

-- | Datatype used for representing and debugging the
-- structure of a router. Abstracts from the functions
-- being used in the actual router and the handlers at
-- the leaves.
--
-- Two 'Router's can be structurally compared by computing
-- their 'RouterStructure' using 'routerStructure' and
-- then testing for equality, see 'sameStructure'.
--
data RouterStructure =
    WithRequestStructure   RouterStructure
  | StaticRouterStructure  (Map Text RouterStructure) Int
  | DynamicRouterStructure RouterStructure
  | RawRouterStructure
  | ChoiceStructure        RouterStructure RouterStructure
  deriving (Eq, Show)

-- | Compute the structure of a router.
--
-- Assumes that the request or text being passed
-- in 'WithRequest' or 'DynamicRouter' does not
-- affect the structure of the underlying tree.
--
routerStructure :: Router' a -> RouterStructure
routerStructure (WithRequest f) =
  WithRequestStructure $
    routerStructure (f (error "routerStructure: dummy request"))
routerStructure (StaticRouter m ls) =
  StaticRouterStructure (fmap routerStructure m) (length ls)
routerStructure (DynamicRouter f) =
  DynamicRouterStructure $
    routerStructure (f (error "routerStructure: dummy text"))
routerStructure (RawRouter _) =
  RawRouterStructure
routerStructure (Choice r1 r2) =
  ChoiceStructure
    (routerStructure r1)
    (routerStructure r2)

-- | Compare the structure of two routers.
--
sameStructure :: Router' a -> Router' b -> Bool
sameStructure r1 r2 =
  routerStructure r1 == routerStructure r2

-- | Provide a textual representation of the
-- structure of a router.
--
routerLayout :: Router' a -> Text
routerLayout router =
  T.unlines (["/"] ++ mkRouterLayout False (routerStructure router))
  where
    mkRouterLayout :: Bool -> RouterStructure -> [Text]
    mkRouterLayout c (WithRequestStructure r)    = mkRouterLayout c r
    mkRouterLayout c (StaticRouterStructure m n) = mkSubTrees c (M.toList m) n
    mkRouterLayout c (DynamicRouterStructure r)  = mkSubTree c "<dyn>" (mkRouterLayout False r)
    mkRouterLayout c  RawRouterStructure         =
      if c then ["├─ <raw>"] else ["└─ <raw>"]
    mkRouterLayout c (ChoiceStructure r1 r2)     =
      mkRouterLayout True r1 ++ ["┆"] ++ mkRouterLayout c r2

    mkSubTrees :: Bool -> [(Text, RouterStructure)] -> Int -> [Text]
    mkSubTrees _ []             0 = []
    mkSubTrees c []             n =
      concat (replicate (n - 1) (mkLeaf True) ++ [mkLeaf c])
    mkSubTrees c [(t, r)]       0 =
      mkSubTree c    t (mkRouterLayout False r)
    mkSubTrees c ((t, r) : trs) n =
      mkSubTree True t (mkRouterLayout False r) ++ mkSubTrees c trs n

    mkLeaf :: Bool -> [Text]
    mkLeaf True  = ["├─•","┆"]
    mkLeaf False = ["└─•"]

    mkSubTree :: Bool -> Text -> [Text] -> [Text]
    mkSubTree True  path children = ("├─ " <> path <> "/") : map ("│  " <>) children
    mkSubTree False path children = ("└─ " <> path <> "/") : map ("   " <>) children

-- | Apply a transformation to the response of a `Router`.
tweakResponse :: (RouteResult Response -> RouteResult Response) -> Router -> Router
tweakResponse f = fmap (\a -> \req cont -> a req (cont . f))

-- | Interpret a router as an application.
runRouter :: Router -> RoutingApplication
runRouter (WithRequest router) request respond =
  runRouter (router request) request respond
runRouter (StaticRouter table ls) request respond =
  case pathInfo request of
    []   -> runChoice ls request respond
    -- This case is to handle trailing slashes.
    [""] -> runChoice ls request respond
    first : rest | Just router <- M.lookup first table
      -> let request' = request { pathInfo = rest }
         in  runRouter router request' respond
    _ -> respond $ Fail err404
runRouter (DynamicRouter fun)  request respond =
  case pathInfo request of
    []   -> respond $ Fail err404
    -- This case is to handle trailing slashes.
    [""] -> respond $ Fail err404
    first : rest
      -> let request' = request { pathInfo = rest }
         in  runRouter (fun first) request' respond
runRouter (RawRouter app)      request respond = app request respond
runRouter (Choice r1 r2)       request respond =
  runChoice [runRouter r1, runRouter r2] request respond

-- | Try a list of routing applications in order.
-- We stop as soon as one fails fatally or succeeds.
-- If all fail normally, we pick the "best" error.
--
runChoice :: [RoutingApplication] -> RoutingApplication
runChoice []       _request respond = respond (Fail err404)
runChoice [r]       request respond = r request respond
runChoice (r : rs)  request respond =
  r request $ \ response1 ->
  case response1 of
    Fail _ -> runChoice rs request $ \ response2 ->
      respond $ highestPri response1 response2
    _      -> respond response1
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
