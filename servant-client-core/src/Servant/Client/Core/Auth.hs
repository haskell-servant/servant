{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Authentication for clients

module Servant.Client.Core.Auth (
    AuthClientData,
    AuthenticatedRequest (..),
    mkAuthenticatedRequest,
    ) where

import           Servant.Client.Core.Request
                 (Request)

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
newtype AuthenticatedRequest a =
  AuthenticatedRequest { unAuthReq :: (AuthClientData a, AuthClientData a -> Request -> Request) }

-- | Handy helper to avoid wrapping datatypes in tuples everywhere.
--
-- NOTE: THIS API IS EXPERIMENTAL AND SUBJECT TO CHANGE
mkAuthenticatedRequest :: AuthClientData a
                  -> (AuthClientData a -> Request -> Request)
                  -> AuthenticatedRequest a
mkAuthenticatedRequest val func = AuthenticatedRequest (val, func)
