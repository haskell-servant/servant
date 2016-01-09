{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Authentication for clients

module Servant.Common.Auth (
    AuthenticateClientRequest ( ClientAuthType, authReq )
  , BasicAuthData (BasicAuthData, username, password)
  , basicAuthReq
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Base64  (encode)
import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8)
import Servant.Common.Req (addHeader, Req)


-- | A simple datatype to hold data required to decorate a request
data BasicAuthData = BasicAuthData { username :: ByteString
                                   , password :: ByteString
                                   }

-- | Authenticate a request using Basic Authentication
basicAuthReq :: BasicAuthData -> Req -> Req
basicAuthReq (BasicAuthData user pass) req =
    let authText = decodeUtf8 ("Basic " <> encode (user <> ":" <> pass))
    in addHeader "Authorization" authText req

-- | Class to represent the ability to authenticate a 'Request'
-- object. For example, we may add special headers to the 'Request'.
class AuthenticateClientRequest a where
  data ClientAuthType a :: *
  authReq :: ClientAuthType a -> Req -> Req
