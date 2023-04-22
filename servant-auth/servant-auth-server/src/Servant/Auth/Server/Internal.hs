{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Auth.Server.Internal where

import           Control.Monad.Except (runExceptT, join)
import           Control.Monad.Trans (liftIO)
import           Data.Kind           (Type)
import           Data.Typeable       (Typeable, typeRep)
import           Network.Wai         (Request, queryString)
import           Servant
import           Servant.API.Modifiers (FoldLenient, FoldRequired, RequestArgument, unfoldRequestArgument)
import           Servant.Auth        (Auth)
import           Servant.Auth.JWT    (ToJWT)
import           Data.Text           (Text)

import Servant.Auth.Server.Internal.AddSetCookie
import Servant.Auth.Server.Internal.Class
import Servant.Auth.Server.Internal.Cookie
import Servant.Auth.Server.Internal.ConfigTypes
-- import Servant.Auth.Server.Internal.JWT
import Servant.Auth.Server.Internal.Types

import Servant.Server.Experimental.Auth (AuthHandler (..))
import Servant.Server.Internal (DelayedIO, addAuthCheck, delayedFail, delayedFailFatal, mkContextWithErrorFormatter, withRequest, MkContextWithErrorFormatter)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import Data.ByteString.Lazy (ByteString)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Network.HTTP.Types (queryToQueryText)
import Data.String (fromString)

instance ( n ~ 'S ('S 'Z)
         , HasServer (AddSetCookiesApi n api) ctxs, AreAuths auths ctxs v
         , HasServer api ctxs -- this constraint is needed to implement hoistServer
         , AddSetCookies n (ServerT api Handler) (ServerT (AddSetCookiesApi n api) Handler)
         , ToJWT v
         , HasContextEntry ctxs CookieSettings
         , HasContextEntry ctxs JWTSettings
         ) => HasServer (Auth auths v :> api) ctxs where
  type ServerT (Auth auths v :> api) m = AuthResult v -> ServerT api m

#if MIN_VERSION_servant_server(0,12,0)
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s
#endif

  route _ context subserver =
    route (Proxy :: Proxy (AddSetCookiesApi n api))
          context
          (fmap go subserver `addAuthCheck` authCheck)

    where
      authCheck :: DelayedIO (AuthResult v, SetCookieList ('S ('S 'Z)))
      authCheck = withRequest $ \req -> liftIO $ do
        authResult <- runAuthCheck (runAuths (Proxy :: Proxy auths) context) req
        cookies <- makeCookies authResult
        return (authResult, cookies)

      jwtSettings :: JWTSettings
      jwtSettings = getContextEntry context

      cookieSettings :: CookieSettings
      cookieSettings = getContextEntry context

      makeCookies :: AuthResult v -> IO (SetCookieList ('S ('S 'Z)))
      makeCookies authResult = do
        case authResult of
          (Authenticated v) -> do
            ejwt <- makeSessionCookie cookieSettings jwtSettings v
            xsrf <- makeXsrfCookie cookieSettings
            return $ Just xsrf `SetCookieCons` (ejwt `SetCookieCons` SetCookieNil)
          _ -> return $ Nothing `SetCookieCons` (Nothing `SetCookieCons` SetCookieNil)

      go :: (AuthResult v -> ServerT api Handler)
         -> (AuthResult v, SetCookieList n)
         -> ServerT (AddSetCookiesApi n api) Handler
      go fn (authResult, cookies) = addSetCookies cookies $ fn authResult


{-
NewAuth is a "quick" PoC to have a more modular way of providing
authentications and the checking thereof.

In the current implementation, all of the 'auths' are checked one
by one, and the first that is present is tried and will either
be returned when successful, or when not, throw an error given
the appropriate 'ErrorFormatter' or return a 'Left err' if the
'Lenient' modifier is set.
Only if NONE of the auths are present will it either throw an
'err401' or, if the 'Optional' modifier is set, return a 'Nothing'.
This error might also be customizeable if we make it be required
from the context.
-}

data NewAuth (mods :: [Type]) (auths :: [Type]) (a :: Type)
    deriving (Typeable)

type NewAuthResult a = Maybe (Either Text a)

instance
    ( HasServer api ctxs
    , HasContextEntry (MkContextWithErrorFormatter ctxs) ErrorFormatters
    , SBoolI (FoldRequired mods)
    , SBoolI (FoldLenient mods)
    , AllAuth auths a
    , HasContextEntry ctxs (AuthHandler Request (NewAuthResult a))
    ) => HasServer (NewAuth mods auths a :> api) ctxs where
    type ServerT (NewAuth mods auths a :> api) m =
        RequestArgument mods a -> ServerT api m
    hoistServerWithContext _ pc nt s =
        hoistServerWithContext (Proxy @api) pc nt . s
    route _ context subserver =
        route (Proxy @api) context $ subserver `addAuthCheck` authCheck
      where
        errorFormatters :: ErrorFormatters
        errorFormatters = getContextEntry $ mkContextWithErrorFormatter context

        authCheck :: DelayedIO (RequestArgument mods a)
        authCheck = withRequest $ \req -> do
            mRes <- processAllAuthHandlers req authHandlers
            case mRes of
                Nothing -> case sbool :: SBool (FoldRequired mods) of
                    STrue -> absent
                    SFalse -> pure Nothing
                Just (badForm, eRes) ->
                    unfoldRequestArgument (Proxy @mods) absent badForm $ Just eRes

        processAllAuthHandlers ::
            Request ->
            [NewAuthHandler Request (NewAuthResult a)] ->
            DelayedIO (Maybe (Text -> DelayedIO (RequestArgument mods a), Either Text a))
        processAllAuthHandlers _req [] = pure Nothing
        processAllAuthHandlers req (auth : auths) = do
            eRes <- liftIO . runExceptT . runHandler' $ getHandler auth req
            either delayedFail go eRes
          where
            go Nothing = processAllAuthHandlers req auths
            go (Just res) = pure $ Just (badForm, res)
            badForm err = delayedFailFatal $ errFmtr rep req . T.unpack $ msg <> err
            errFmtr = getErrorFormatter auth errorFormatters
            rep = typeRep (Proxy :: Proxy NewAuth)
            msg = "Authentication via " <> getAuthName auth <> " failed: "


        authHandlers :: [NewAuthHandler Request (NewAuthResult a)]
        authHandlers = allAuthHandlers (Proxy @auths)

        toLBS :: Text -> ByteString
        toLBS = TLE.encodeUtf8 . TL.fromStrict

        absent :: DelayedIO (RequestArgument mods a)
        absent =
            delayedFailFatal $ err401{ errBody = toLBS msg }
          where
            allAuthMethodNames = getAuthName <$> authHandlers
            msg = case allAuthMethodNames of
                [] -> "No authentication required, something went wrong."
                [auth] -> "Authentication required: " <> auth
                auths -> "One of the following authentications required: " <> T.intercalate ", " auths

data NewAuthHandler r res = NewAuthHandler
    { getHandler :: r -> Handler res
    -- Used in the following errors:
    -- "One of the following authentications required: {authName}, {authName}, etc."
    -- And
    -- "Authentication via {authName}: (Left Text)"
    , getAuthName :: Text
    , getErrorFormatter :: ErrorFormatters -> ErrorFormatter
    } deriving Typeable

class AllAuth (auths :: [Type]) a where
    allAuthHandlers :: proxy auths -> [NewAuthHandler Request (NewAuthResult a)]

instance AllAuth '[] a where
    allAuthHandlers _ = []

instance (AllAuth auths a, HasAuthHandler auth a) => AllAuth (auth ': auths) a where
    allAuthHandlers _ = getAuthHandler (Proxy @auth) : allAuthHandlers (Proxy @auths)

class HasAuthHandler auth a where
    getAuthHandler :: proxy auth -> NewAuthHandler Request (NewAuthResult a)

{-
The following is an example of a partial implementation to be used by users
where they will only have to supply a 'FromJWT' instance for their type.
There's a lot of possibilities, but this is just a quick and easy example.
-}

-- | Designates a query parameter named @sym@ which should contain a valid JWT
--
-- E.g. @JWTQueryParam "token"@ will try to parse the value from the query
-- parameter named "token" in the path (i.e. "example.com/path?token=...")
data JWTQueryParam sym

-- | Pretty library agnostic way of providing a JWT API, but less plug'n'play.
--
-- The Servant.Auth.JWT 'FromJWT' forces the 'Value' to be in the "dat" field,
-- but makes this implementation easier and user-friendly. So it depends on
-- what we'd prefer.
class FromJWT a where
    parseJWT :: Text -> Either Text a

instance (FromJWT a, KnownSymbol sym) => HasAuthHandler (JWTQueryParam sym) a where
    getAuthHandler _ = NewAuthHandler
        { getHandler = \req ->
            let paramname = fromString $ symbolVal (Proxy :: Proxy sym)
                mev = join . lookup paramname . queryToQueryText $ queryString req
            in pure $ parseJWT <$> mev
        , getAuthName = "JWT Query Parameter"
        , getErrorFormatter = urlParseErrorFormatter
        }
