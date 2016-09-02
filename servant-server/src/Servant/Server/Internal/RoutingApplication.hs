{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
module Servant.Server.Internal.RoutingApplication where

import           Control.Monad                      (ap, liftM)
import           Control.Monad.Trans                (MonadIO(..))
import           Control.Monad.Trans.Except         (runExceptT)
import           Network.Wai                        (Application, Request,
                                                     Response, ResponseReceived)
import           Prelude                            ()
import           Prelude.Compat
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

toApplication :: RoutingApplication -> Application
toApplication ra request respond = ra request routingRespond
 where
  routingRespond :: RouteResult Response -> IO ResponseReceived
  routingRespond (Fail err)      = respond $ responseServantErr err
  routingRespond (FailFatal err) = respond $ responseServantErr err
  routingRespond (Route v)       = respond v

-- | A 'Delayed' is a representation of a handler with scheduled
-- delayed checks that can trigger errors.
--
-- Why would we want to delay checks?
--
-- There are two reasons:
--
-- 1. In a straight-forward implementation, the order in which we
-- perform checks will determine the error we generate. This is
-- because once an error occurs, we would abort and not perform
-- any subsequent checks, but rather return the current error.
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
data Delayed env c where
  Delayed :: { capturesD :: env -> DelayedIO captures
             , methodD   :: DelayedIO ()
             , authD     :: DelayedIO auth
             , bodyD     :: DelayedIO body
             , serverD   :: captures -> auth -> body -> Request -> RouteResult c
             } -> Delayed env c

instance Functor (Delayed env) where
  fmap f Delayed{..} =
    Delayed
      { serverD = \ c a b req -> f <$> serverD c a b req
      , ..
      } -- Note [Existential Record Update]

-- | Computations used in a 'Delayed' can depend on the
-- incoming 'Request', may perform 'IO, and result in a
-- 'RouteResult, meaning they can either suceed, fail
-- (with the possibility to recover), or fail fatally.
--
newtype DelayedIO a = DelayedIO { runDelayedIO :: Request -> IO (RouteResult a) }

instance Functor DelayedIO where
  fmap = liftM

instance Applicative DelayedIO where
  pure = return
  (<*>) = ap

instance Monad DelayedIO where
  return x = DelayedIO (const $ return (Route x))
  DelayedIO m >>= f =
    DelayedIO $ \ req -> do
      r <- m req
      case r of
        Fail      e -> return $ Fail e
        FailFatal e -> return $ FailFatal e
        Route     a -> runDelayedIO (f a) req

instance MonadIO DelayedIO where
  liftIO m = DelayedIO (const $ Route <$> m)

-- | A 'Delayed' without any stored checks.
emptyDelayed :: RouteResult a -> Delayed env a
emptyDelayed result =
  Delayed (const r) r r r (\ _ _ _ _ -> result)
  where
    r = return ()

-- | Fail with the option to recover.
delayedFail :: ServantErr -> DelayedIO a
delayedFail err = DelayedIO (const $ return $ Fail err)

-- | Fail fatally, i.e., without any option to recover.
delayedFailFatal :: ServantErr -> DelayedIO a
delayedFailFatal err = DelayedIO (const $ return $ FailFatal err)

-- | Gain access to the incoming request.
withRequest :: (Request -> DelayedIO a) -> DelayedIO a
withRequest f = DelayedIO (\ req -> runDelayedIO (f req) req)

-- | Add a capture to the end of the capture block.
addCapture :: Delayed env (a -> b)
           -> (captured -> DelayedIO a)
           -> Delayed (captured, env) b
addCapture Delayed{..} new =
  Delayed
    { capturesD = \ (txt, env) -> (,) <$> capturesD env <*> new txt
    , serverD   = \ (x, v) a b req -> ($ v) <$> serverD x a b req
    , ..
    } -- Note [Existential Record Update]

-- | Add a method check to the end of the method block.
addMethodCheck :: Delayed env a
               -> DelayedIO ()
               -> Delayed env a
addMethodCheck Delayed{..} new =
  Delayed
    { methodD = methodD <* new
    , ..
    } -- Note [Existential Record Update]

-- | Add an auth check to the end of the auth block.
addAuthCheck :: Delayed env (a -> b)
             -> DelayedIO a
             -> Delayed env b
addAuthCheck Delayed{..} new =
  Delayed
    { authD   = (,) <$> authD <*> new
    , serverD = \ c (y, v) b req -> ($ v) <$> serverD c y b req
    , ..
    } -- Note [Existential Record Update]

-- | Add a body check to the end of the body block.
addBodyCheck :: Delayed env (a -> b)
             -> DelayedIO a
             -> Delayed env b
addBodyCheck Delayed{..} new =
  Delayed
    { bodyD   = (,) <$> bodyD <*> new
    , serverD = \ c a (z, v) req -> ($ v) <$> serverD c a z req
    , ..
    } -- Note [Existential Record Update]


-- | Add an accept header check to the beginning of the body
-- block. There is a tradeoff here. In principle, we'd like
-- to take a bad body (400) response take precedence over a
-- failed accept check (406). BUT to allow streaming the body,
-- we cannot run the body check and then still backtrack.
-- We therefore do the accept check before the body check,
-- when we can still backtrack. There are other solutions to
-- this, but they'd be more complicated (such as delaying the
-- body check further so that it can still be run in a situation
-- where we'd otherwise report 406).
addAcceptCheck :: Delayed env a
               -> DelayedIO ()
               -> Delayed env a
addAcceptCheck Delayed{..} new =
  Delayed
    { bodyD = new *> bodyD
    , ..
    } -- Note [Existential Record Update]

-- | Many combinators extract information that is passed to
-- the handler without the possibility of failure. In such a
-- case, 'passToServer' can be used.
passToServer :: Delayed env (a -> b) -> (Request -> a) -> Delayed env b
passToServer Delayed{..} x =
  Delayed
    { serverD = \ c a b req -> ($ x req) <$> serverD c a b req
    , ..
    } -- Note [Existential Record Update]

-- | Run a delayed server. Performs all scheduled operations
-- in order, and passes the results from the capture and body
-- blocks on to the actual handler.
--
-- This should only be called once per request; otherwise the guarantees about
-- effect and HTTP error ordering break down.
runDelayed :: Delayed env a
           -> env
           -> Request
           -> IO (RouteResult a)
runDelayed Delayed{..} env = runDelayedIO $ do
  c <- capturesD env
  methodD
  a <- authD
  b <- bodyD
  DelayedIO (\ req -> return $ serverD c a b req)

-- | Runs a delayed server and the resulting action.
-- Takes a continuation that lets us send a response.
-- Also takes a continuation for how to turn the
-- result of the delayed server into a response.
runAction :: Delayed env (Handler a)
          -> env
          -> Request
          -> (RouteResult Response -> IO r)
          -> (a -> RouteResult Response)
          -> IO r
runAction action env req respond k =
  runDelayed action env req >>= go >>= respond
  where
    go (Fail e)      = return $ Fail e
    go (FailFatal e) = return $ FailFatal e
    go (Route a)     = do
      e <- runExceptT a
      case e of
        Left err -> return . Route $ responseServantErr err
        Right x  -> return $! k x

{- Note [Existential Record Update]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Due to GHC issue <https://ghc.haskell.org/trac/ghc/ticket/2595 2595>, we cannot
do the more succint thing - just update the records we actually change.
-}
