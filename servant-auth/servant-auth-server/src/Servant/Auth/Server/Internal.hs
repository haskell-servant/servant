{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Auth.Server.Internal where

import Control.Monad.Trans (liftIO)
import Servant
  ( Handler
  , HasContextEntry (getContextEntry)
  , HasServer (..)
  , Proxy (..)
  , (:>)
  )
import Servant.Auth
import Servant.Auth.JWT (ToJWT)
import Servant.Server.Internal (DelayedIO, addAuthCheck, withRequest)

import Servant.Auth.Server.Internal.AddSetCookie
import Servant.Auth.Server.Internal.Class
import Servant.Auth.Server.Internal.ConfigTypes
import Servant.Auth.Server.Internal.Cookie
import Servant.Auth.Server.Internal.JWT
import Servant.Auth.Server.Internal.Types

instance
  ( -- this constraint is needed to implement hoistServer
    AddSetCookies n (ServerT api Handler) (ServerT (AddSetCookiesApi n api) Handler)
  , AreAuths auths ctxs v
  , HasContextEntry ctxs CookieSettings
  , HasContextEntry ctxs JWTSettings
  , HasServer (AddSetCookiesApi n api) ctxs
  , HasServer api ctxs
  , ToJWT v
  , n ~ 'S ('S 'Z)
  )
  => HasServer (Auth auths v :> api) ctxs
  where
  type ServerT (Auth auths v :> api) m = AuthResult v -> ServerT api m

  route _ context subserver =
    route
      (Proxy :: Proxy (AddSetCookiesApi n api))
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

      go
        :: (AuthResult v -> ServerT api Handler)
        -> (AuthResult v, SetCookieList n)
        -> ServerT (AddSetCookiesApi n api) Handler
      go fn (authResult, cookies) = addSetCookies cookies $ fn authResult

#if MIN_VERSION_servant_server(0,12,0)
  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s
#endif
