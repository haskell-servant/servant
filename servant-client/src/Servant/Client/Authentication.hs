-- | Authentication for clients

module Servant.Client.Authentication (
    AuthenticateRequest ( authReq )
    ) where

import Servant.Common.Req (Req)

-- | Class to represent the ability to authenticate a 'Request'
-- object. For example, we may add special headers to the 'Request'.
class AuthenticateRequest a where
    authReq :: a -> Req -> Req
