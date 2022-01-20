module Servant.Server.Internal.Router.Prometheus where

-- | The 'RoutingApplication' variant of 'instrumentHandlerValue'
instrumentRoutingHandlerValue :: (Request -> Text) -> RoutingApplication -> RoutingApplication
instrumentRoutingHandlerValue getHandlerLabel app = \req respond -> do
    start <- getTime Monotonic
    app req $ \res -> do
        end <- getTime Monotonic
        let method = Just $ decodeUtf8 (Wai.requestMethod req)
        let status = Just $ T.pack (show (HTTP.statusCode (Wai.responseStatus res')))
        observeSeconds (getHandlerLabel req) method status start end
        respond res


-- Traverses the router tree and adds the path to the parent to each leaf node.
-- This makes each leaf aware of the routing path. This is useful to add metrics
-- for each path. Where you want to annotate the metric with the route that was
-- matched
routerWithPaths :: Router env -> Router ([Text], env)
routerWithPaths = go []
    where
        go :: [Text] -> Router env -> Router ([Text], env)
        go path router =
            case router of
                StaticRouter children leafs ->
                    StaticRouter
                        handleChildren children
                        (List.map handleLeaf leafs)
                CaptureRouter captureRouter ->
                    -- We  discard the capture as we don't want arbitrary data
                    -- as labels. Say the capture is a user ID this would cause
                    -- every user to get its own metric and blow up prometheus.
                    -- Instead we group all user queries together.  e.g.
                    -- /users/24284/befriend and /users/95493/befriend will both
                    -- be displayed as the metric label
                    -- "users/:capture/befriend"
                    fmap (\_capture -> path ++ [":capture"]) captureRouter
                CaptureAllRouter captureAllRouter -> 
                    -- Same as above. But CaptureAll eats all remaining path components. Represented by the * wildcard
                    fmap (\_captures -> path ++ ["*"]) captureAllRouter
                RawRouter leaf ->
                    handleLeaf leaf 
                Choice left right -> 
                    Choice (go path left) (go path right)

        -- | The recursive step. Each element of the map gets its path part that
        -- it handles appended to the path
        handleChildren :: Map Text (Router env) -> Map Text (Router ([Text], env))
        handleChildren children = 
            -- at this point we already traversed 'paths' ~  ["api","v1"]
            -- We now want:
            --   "users" -> userRouter        =>  "users" -> userRouter( ["api", "v1", "users"])
            --   "payments" -> paymentRouter  =>  "payments" -> paymentRouter (["api", "v1", "payments"])
            Map.mapWithKey appendRoute children
            where
                appendRoute :: Text -> Router env -> Router ([Text], env)
                appendRoute pathPart = go (path ++ [pathPart])

        -- | A leaf is handled by running the application whilst instrumenting
        -- it; labelling it with the path in the routing tree
        handleLeaf :: (env -> RoutingApplication) -> ([Text], env) -> RoutingApplication
        handleLeaf runApp (path, env) =
            instrumentRoutingHandlerValue (makeLabel path) (runApp env)
            where
                makeLabel :: [Text] -> Request -> Text
                makeLabel paths _ = Text.intercalate "/" paths

            
instrumentLeaf :: (([Text], env) -> RoutingApplication) -> ([Text], env) -> RoutingApplication
instrumentLeaf runApp (path, env) = 
    -- TODO: should we tie in the start time of the request, so we can
    -- include the duration of the actual routing algorithm in the
    -- results?
    instrumentRoutingHandleValue
        -- TODO: Should we apppend what's still in the request? Lets add a test to figure out :)
        (\paths _req -> Text.intercalate "/" paths)
        (runApp env)

-- NOTE: We can pass the start time in the env. That way we know how long routing took

-- | Takes a servant 'Router' and adds prometheus metrics to each handler for each route.
-- This is useful to add instrumentation to all your handlers automatically.
-- Note that the time it takes to execute the routing to the endpoint is not included in the metrics.
-- If we want that, we'd have to reimplement 'runRouterEnv'. 
-- It builds up the labels as the routes are being traversed, such that we can label the metric correctly.
routerWithMetrics :: Router ([Text], env)  -> Router ([Text],env) 
routerWithMetrics (StaticRouter children leafs) =
    StaticRouter (Map.map routerWithMetrics children) (List.map instrumentLeaf leafs)
routerWithMetrics (CaptureRouter router) =
    CaptureRouter (routerWithMetrics router)
routerWithMetrics (CaptureAllRouter router) =
    CaptuerAllRouter (routerWithMetrics router)
routerWithMetrics (RawRouter runApp) =
    RawRouter (instrumentLeaf runApp)
routerWithMetrics (Choice left right) =
    Choice (routerWithMetrics left) (routerWithMetrics right)
