{-# LANGUAGE OverloadedStrings #-}

-- | Authentication for clients

module Servant.Client.Authentication (
    AuthenticateRequest ( authReq )
    ) where

import Data.ByteString.Base64  (encode)
import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8)
import Servant.API.Authentication (BasicAuth(BasicAuth))
import Servant.Common.Req (addHeader, Req)

-- | Class to represent the ability to authenticate a 'Request'
-- object. For example, we may add special headers to the 'Request'.
class AuthenticateRequest a where
    authReq :: a -> Req -> Req
     

instance AuthenticateRequest (BasicAuth realm) where
    authReq (BasicAuth user pass) req =
        let authText = decodeUtf8 ("Basic " <> encode (user <> ":" <> pass)) in
            addHeader "Authorization" authText req

