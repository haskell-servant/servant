{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
module Servant.Server.Internal.RoutingApplication where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative                (Applicative, (<$>))
import           Data.Monoid                        (Monoid, mappend, mempty)
#endif
import           Control.Monad.Trans.Except         (ExceptT, runExceptT)
import qualified Data.ByteString                    as B
import qualified Data.ByteString.Lazy               as BL
import           Data.IORef                         (newIORef, readIORef,
                                                     writeIORef)
import           Data.Maybe                         (fromMaybe)
import           Data.Monoid                        ((<>))
import           Data.String                        (fromString)
import           Network.HTTP.Types                 hiding (ResponseHeaders)
import           Network.Wai                        (Application, Request,
                                                     Response, ResponseReceived,
                                                     requestBody, responseLBS,
                                                     strictRequestBody)
import           Servant.API                        ((:<|>) (..))
import           Servant.Server.Internal.ServantErr

type RoutingApplication =
     Request -- ^ the request, the field 'pathInfo' may be modified by url routing
  -> (RouteResult Response -> IO ResponseReceived) -> IO ResponseReceived

-- | A wrapper around @'Either' 'RouteMismatch' a@.
newtype RouteResult a =
  RR { routeResult :: Either RouteMismatch a }
  deriving (Functor, Applicative, Monad)

-- | If we get a `Right`, it has precedence over everything else.
--
-- This in particular means that if we could get several 'Right's,
-- only the first we encounter would be taken into account.
instance Monoid (RouteResult a) where
  mempty = RR $ Left mempty

  RR (Left x)  `mappend` RR (Left y)  = RR $ Left (x <> y)
  RR (Left _)  `mappend` RR (Right y) = RR $ Right y
  r            `mappend` _            = r

-- Note that the ordering of the constructors has great significance! It
-- determines the Ord instance and, consequently, the monoid instance.
data RouteMismatch =
    NotFound           -- ^ the usual "not found" error
  | WrongMethod        -- ^ a more informative "you just got the HTTP method wrong" error
  | UnsupportedMediaType -- ^ request body has unsupported media type
  | InvalidBody String -- ^ an even more informative "your json request body wasn't valid" error
  | HttpError Status [Header] (Maybe BL.ByteString)  -- ^ an even even more informative arbitrary HTTP response code error.
  | RouteMismatch Response -- ^ an arbitrary mismatch with custom Response.

-- | specialized 'Less Than' for use with Monoid RouteMismatch
(<=:) :: RouteMismatch -> RouteMismatch -> Bool
{-# INLINE (<=:) #-}
NotFound             <=: _   = True
WrongMethod          <=: rmm = not (rmm <=: NotFound)
UnsupportedMediaType <=: rmm = not (rmm <=: WrongMethod)
InvalidBody _        <=: rmm = not (rmm <=: UnsupportedMediaType)
HttpError _ _ _      <=: rmm = not (rmm <=: (InvalidBody ""))
RouteMismatch _      <=: _   = False

instance Monoid RouteMismatch where
  mempty = NotFound
  -- The following isn't great, since it picks @InvalidBody@ based on
  -- alphabetical ordering, but any choice would be arbitrary.
  --
  -- "As one judge said to the other, 'Be just and if you can't be just, be
  -- arbitrary'" -- William Burroughs
  --
  -- It used to be the case that `mappend = max` but getting rid of the `Eq`
  -- and `Ord` instance meant we had to roll out our own max ;\
  rmm                  `mappend` NotFound                           = rmm
  NotFound             `mappend` rmm                                = rmm
  WrongMethod          `mappend` rmm | rmm <=: WrongMethod          = WrongMethod
  WrongMethod          `mappend` rmm                                = rmm
  UnsupportedMediaType `mappend` rmm | rmm <=: UnsupportedMediaType = UnsupportedMediaType
  UnsupportedMediaType `mappend` rmm                                = rmm
  i@(InvalidBody _)    `mappend` rmm | rmm <=: i                    = i
  InvalidBody _        `mappend` rmm                                = rmm
  h@(HttpError _ _ _)  `mappend` rmm | rmm <=: h                    = h
  HttpError _ _ _      `mappend` rmm                                = rmm
  r@(RouteMismatch _)  `mappend` _                                  = r

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

  ra request{ requestBody = memoReqBody } (routingRespond . routeResult)
 where
  routingRespond :: Either RouteMismatch Response -> IO ResponseReceived
  routingRespond (Left NotFound) =
    respond $ responseLBS notFound404 [] "not found"
  routingRespond (Left WrongMethod) =
    respond $ responseLBS methodNotAllowed405 [] "method not allowed"
  routingRespond (Left (InvalidBody err)) =
    respond $ responseLBS badRequest400 [] $ fromString $ "invalid request body: " ++ err
  routingRespond (Left UnsupportedMediaType) =
    respond $ responseLBS unsupportedMediaType415 [] "unsupported media type"
  routingRespond (Left (HttpError status headers body)) =
    respond $ responseLBS status headers $ fromMaybe (BL.fromStrict $ statusMessage status) body
  routingRespond (Left (RouteMismatch resp)) =
    respond resp
  routingRespond (Right response) =
    respond response

runAction :: IO (RouteResult (ExceptT ServantErr IO a))
          -> (RouteResult Response -> IO r)
          -> (a -> RouteResult Response)
          -> IO r
runAction action respond k = do
  r <- action
  go r
  where
    go (RR (Right a))  = do
      e <- runExceptT a
      respond $ case e of
        Right x  -> k x
        Left err -> succeedWith $ responseServantErr err
    go (RR (Left err)) = respond $ failWith err

feedTo :: IO (RouteResult (a -> b)) -> a -> IO (RouteResult b)
feedTo f x = (($ x) <$>) <$> f

extractL :: RouteResult (a :<|> b) -> RouteResult a
extractL (RR (Right (a :<|> _))) = RR (Right a)
extractL (RR (Left err))         = RR (Left err)

extractR :: RouteResult (a :<|> b) -> RouteResult b
extractR (RR (Right (_ :<|> b))) = RR (Right b)
extractR (RR (Left err))         = RR (Left err)

failWith :: RouteMismatch -> RouteResult a
failWith = RR . Left

succeedWith :: a -> RouteResult a
succeedWith = RR . Right

isMismatch :: RouteResult a -> Bool
isMismatch (RR (Left _)) = True
isMismatch _             = False

