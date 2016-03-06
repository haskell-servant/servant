{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Authentication for clients

module Servant.Client.Experimental.Auth (
    AuthenticateReq(AuthenticateReq, unAuthReq)
  , AuthClientData
  , mkAuthenticateReq
  ) where

import Servant.Common.Req (Req)

-- | For a resource protected by authentication (e.g. AuthProtect), we need
-- to provide the client with some data used to add authentication data
-- to a request
--
-- NOTE: THIS API IS EXPERIMENTAL AND SUBJECT TO CHANGE
type family AuthClientData a :: *

-- | For better type inference and to avoid usage of a data family, we newtype
-- wrap the combination of some 'AuthClientData' and a function to add authentication
-- data to a request
--
-- NOTE: THIS API IS EXPERIMENTAL AND SUBJECT TO CHANGE
newtype AuthenticateReq a =
  AuthenticateReq { unAuthReq :: (AuthClientData a, AuthClientData a -> Req -> Req) }

-- | Handy helper to avoid wrapping datatypes in tuples everywhere.
--
-- NOTE: THIS API IS EXPERIMENTAL AND SUBJECT TO CHANGE
mkAuthenticateReq :: AuthClientData a
                  -> (AuthClientData a -> Req -> Req)
                  -> AuthenticateReq a
mkAuthenticateReq val func = AuthenticateReq (val, func)
