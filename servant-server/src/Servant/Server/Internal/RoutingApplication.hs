{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Servant.Server.Internal.RoutingApplication where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative                ((<$>))
#endif
import           Control.Monad.Trans.Except         (ExceptT, runExceptT)
import qualified Data.ByteString                    as B
import qualified Data.ByteString.Lazy               as BL
import           Data.IORef                         (newIORef, readIORef,
                                                     writeIORef)
import           Network.Wai                        (Application, Request,
                                                     Response, ResponseReceived,
                                                     requestBody,
                                                     strictRequestBody)
import           Servant.API.Authentication (AuthPolicy(Strict,Lax))
import           Servant.Server.Internal.Authentication
import           Servant.Server.Internal.ServantErr

type RoutingApplication =
     Request -- ^ the request, the field 'pathInfo' may be modified by url routing
  -> (RouteResult Response -> IO ResponseReceived) -> IO ResponseReceived

-- | The result of matching against a path in the route tree.
data RouteResult a =
    Fail ServantErr           -- ^ Keep trying other paths. The @ServantErr@
                              -- should only be 404, 405 or 406.
  | FailFatal !ServantErr     -- ^ Don't try other paths.
  | Route !a
  deriving (Eq, Show, Read, Functor)

data ReqBodyState = Uncalled
                  | Called !B.ByteString
                  | Done !B.ByteString

toApplication :: RoutingApplication -> Application
toApplication ra request respond = do
  reqBodyRef <- newIORef Uncalled
  -- We may need to consume the requestBody more than once.  In order to
  -- maintain the illusion that 'requestBody' works as expected,
  -- 'ReqBodyState' is introduced, and the complete body is memoized and
  -- returned as many times as requested with empty "Done" marker chunks in
  -- between.
  -- See https://github.com/haskell-servant/servant/issues/3
  let memoReqBody = do
          ior <- readIORef reqBodyRef
          case ior of
            Uncalled -> do
                r <- BL.toStrict <$> strictRequestBody request
                writeIORef reqBodyRef $ Done r
                return r
            Called bs -> do
                writeIORef reqBodyRef $ Done bs
                return bs
            Done bs -> do
                writeIORef reqBodyRef $ Called bs
                return B.empty

  ra request{ requestBody = memoReqBody } routingRespond
 where
  routingRespond :: RouteResult Response -> IO ResponseReceived
  routingRespond (Fail err)      = respond $ responseServantErr err
  routingRespond (FailFatal err) = respond $ responseServantErr err
  routingRespond (Route v)       = respond v

-- We currently mix up the order in which we perform checks
-- and the priority with which errors are reported.
--
-- For example, we perform Capture checks prior to method checks,
-- and therefore get 404 before 405.
--
-- However, we also perform body checks prior to method checks
-- now, and therefore get 415 before 405, which is wrong.
--
-- If we delay Captures, but perform method checks eagerly, we
-- end up potentially preferring 405 over 404, whcih is also bad.
--
-- So in principle, we'd like:
--
-- static routes (can cause 404)
-- delayed captures (can cause 404)
-- methods (can cause 405)
-- delayed body (can cause 415, 400)
-- accept header (can cause 406)
--
-- According to the HTTP decision diagram, the priority order
-- between HTTP status codes is as follows:
--

-- | A 'Delayed' is a representation of a handler with scheduled
-- delayed checks that can trigger errors.
--
-- Why would we want to delay checks?
--
-- There are two reasons:
--
-- 1. Currently, the order in which we perform checks coincides
-- with the error we will generate. This is because during checks,
-- once an error occurs, we do not perform any subsequent checks,
-- but rather return this error.
--
-- This is not a necessity: we could continue doing other checks,
-- and choose the preferred error. However, that would in general
-- mean more checking, which leads us to the other reason.
--
-- 2. We really want to avoid doing certain checks too early. For
-- example, captures involve parsing, and are much more costly
-- than static route matches. In particular, if several paths
-- contain the "same" capture, we'd like as much as possible to
-- avoid trying the same parse many times. Also tricky is the
-- request body. Again, this involves parsing, but also, WAI makes
-- obtaining the request body a side-effecting operation. We
-- could/can work around this by manually caching the request body,
-- but we'd rather keep the number of times we actually try to
-- decode the request body to an absolute minimum.
--
-- We prefer to have the following relative priorities of error
-- codes:
--
-- @
-- 404
-- 405 (bad method)
-- 401 (unauthorized)
-- 415 (unsupported media type)
-- 400 (bad request)
-- 406 (not acceptable)
-- @
--
-- Therefore, while routing, we delay most checks so that they
-- will ultimately occur in the right order.
--
-- A 'Delayed' contains three delayed blocks of tests, and
-- the actual handler:
--
-- 1. Delayed captures. These can actually cause 404, and
-- while they're costly, they should be done first among the
-- delayed checks (at least as long as we do not decouple the
-- check order from the error reporting, see above). Delayed
-- captures can provide inputs to the actual handler.
--
-- 2. Method check(s). This can cause a 405. On success,
-- it does not provide an input for the handler. Method checks
-- are comparatively cheap.
--
-- 3. Body and accept header checks. The request body check can
-- cause both 400 and 415. This provides an input to the handler.
-- The accept header check can be performed as the final
-- computation in this block. It can cause a 406.
--
data Delayed :: * -> * where
  Delayed :: IO (RouteResult a)
          -> IO (RouteResult ())
          -> IO (RouteResult b)
          -> IO (RouteResult c)
          -> (a -> b -> c -> RouteResult d)
          -> Delayed d

instance Functor Delayed where
   fmap f (Delayed a b c d g) = Delayed a b c d ((fmap.fmap.fmap.fmap) f g)

-- | Add a capture to the end of the capture block.
addCapture :: Delayed (a -> b)
           -> IO (RouteResult a)
           -> Delayed b
addCapture (Delayed captures method auth body server) new =
  Delayed (combineRouteResults (,) captures new) method auth body (\ (x, v) y z -> ($ v) <$> server x y z)

-- | Add a method check to the end of the method block.
addMethodCheck :: Delayed a
               -> IO (RouteResult ())
               -> Delayed a
addMethodCheck (Delayed captures method auth body server) new =
  Delayed captures (combineRouteResults const method new) auth body server

-- | Add a method to perform authorization in strict mode.
addAuthStrictCheck :: Delayed (AuthProtected auth usr (usr -> a) 'Strict)
                  -> IO (RouteResult (Maybe auth))
                  -> Delayed a
addAuthStrictCheck delayed@(Delayed captures method _ body _) new =
    let newAuth = runDelayed delayed `bindRouteResults` \authProtectionStrict -> new `bindRouteResults` \mAuthData -> case mAuthData of

            Nothing -> do
                -- we're in strict mode: don't let the request go
                -- call the provided "on missing auth" handler
                resp <- onMissingAuthData (authHandlers authProtectionStrict)
                return $ FailFatal resp

            -- successfully pulled auth data out of the request
            Just aData -> do
                mUsr <- (checkAuthStrict authProtectionStrict) aData
                case mUsr of
                    -- this user is not authenticated
                    Nothing -> do
                        resp <- onUnauthenticated (authHandlers authProtectionStrict) aData
                        return $ FailFatal resp

                    -- this user is authenticated
                    Just usr ->
                        (return . Route . subServerStrict authProtectionStrict) usr
    in Delayed captures method newAuth body (\_ y _ -> Route y)

-- | Add a method to perform authorization in strict mode.
addAuthLaxCheck :: Delayed (AuthProtected auth usr (Maybe usr -> a) 'Lax)
                  -> IO (RouteResult (Maybe auth))
                  -> Delayed a
addAuthLaxCheck delayed@(Delayed captures method _ body _) new =
    let newAuth = runDelayed delayed `bindRouteResults` \authProtectionLax -> new `bindRouteResults` \mAuthData ->
            fmap (Route . subServerLax authProtectionLax)
                 (maybe (pure Nothing) (checkAuthLax authProtectionLax) mAuthData)

    in Delayed captures method newAuth body (\_ y _ -> Route y)

-- | Add a body check to the end of the body block.
addBodyCheck :: Delayed (a -> b)
             -> IO (RouteResult a)
             -> Delayed b
addBodyCheck (Delayed captures method auth body server) new =
  Delayed captures method auth (combineRouteResults (,) body new) (\ x y (z, v) -> ($ v) <$> server x y z)

-- | Add an accept header check to the end of the body block.
-- The accept header check should occur after the body check,
-- but this will be the case, because the accept header check
-- is only scheduled by the method combinators.
addAcceptCheck :: Delayed a
                -> IO (RouteResult ())
                -> Delayed a
addAcceptCheck (Delayed captures method auth body server) new =
  Delayed captures method auth (combineRouteResults const body new) server

-- | Many combinators extract information that is passed to
-- the handler without the possibility of failure. In such a
-- case, 'passToServer' can be used.
passToServer :: Delayed (a -> b) -> a -> Delayed b
passToServer d x = ($ x) <$> d

-- | The combination 'IO . RouteResult' is a monad, but we
-- don't explicitly wrap it in a newtype in order to make it
-- an instance. This is the '>>=' of that monad.
--
-- We stop on the first error.
bindRouteResults :: IO (RouteResult a) -> (a -> IO (RouteResult b)) -> IO (RouteResult b)
bindRouteResults m f = do
  r <- m
  case r of
    Fail      e -> return $ Fail e
    FailFatal e -> return $ FailFatal e
    Route     a -> f a

-- | Common special case of 'bindRouteResults', corresponding
-- to 'liftM2'.
combineRouteResults :: (a -> b -> c) -> IO (RouteResult a) -> IO (RouteResult b) -> IO (RouteResult c)
combineRouteResults f m1 m2 =
  m1 `bindRouteResults` \ a ->
  m2 `bindRouteResults` \ b ->
  return (Route (f a b))

-- | Run a delayed server. Performs all scheduled operations
-- in order, and passes the results from the capture and body
-- blocks on to the actual handler.
runDelayed :: Delayed a
           -> IO (RouteResult a)
runDelayed (Delayed captures method auth body server) =
  captures `bindRouteResults` \ c ->
  method   `bindRouteResults` \ _ ->
  auth     `bindRouteResults` \ a ->
  body     `bindRouteResults` \ b ->
  return (server c a b)

-- | Runs a delayed server and the resulting action.
-- Takes a continuation that lets us send a response.
-- Also takes a continuation for how to turn the
-- result of the delayed server into a response.
runAction :: Delayed (ExceptT ServantErr IO a)
          -> (RouteResult Response -> IO r)
          -> (a -> RouteResult Response)
          -> IO r
runAction action respond k = runDelayed action >>= go >>= respond
  where
    go (Fail  e)   = return $ Fail e
    go (FailFatal e) = return $ FailFatal e
    go (Route a)   = do
      e <- runExceptT a
      case e of
        Left err -> return . Route $ responseServantErr err
        Right x  -> return $! k x
