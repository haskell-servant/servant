{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Authentication for clients

module Servant.Common.Auth (
    AuthenticateReq(AuthenticateReq, unAuthReq)
  , AuthClientData
  , basicAuthReq
  , mkAuthenticateReq
  ) where

import Data.ByteString.Base64  (encode)
import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8)
import Servant.Common.Req (addHeader, Req)
import Servant.API.Auth (BasicAuthData(BasicAuthData))

-- | Authenticate a request using Basic Authentication
basicAuthReq :: BasicAuthData -> Req -> Req
basicAuthReq (BasicAuthData user pass) req =
    let authText = decodeUtf8 ("Basic " <> encode (user <> ":" <> pass))
    in addHeader "Authorization" authText req

-- | For a resource protected by authentication (e.g. AuthProtect), we need
-- to provide the client with some data used to add authentication data
-- to a request
type family AuthClientData a :: *

-- | For better type inference and to avoid usage of a data family, we newtype
-- wrap the combination of some 'AuthClientData' and a function to add authentication
-- data to a request
newtype AuthenticateReq a =
  AuthenticateReq { unAuthReq :: (AuthClientData a, AuthClientData a -> Req -> Req) }

-- | Handy helper to avoid wrapping datatypes in tuples everywhere.
mkAuthenticateReq :: AuthClientData a
                  -> (AuthClientData a -> Req -> Req)
                  -> AuthenticateReq a
mkAuthenticateReq val func = AuthenticateReq (val, func)

