{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Basic Authentication for clients

module Servant.Client.Core.Internal.BasicAuth where

import Data.ByteString.Base64 (encode)
import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8)
import Servant.Client.Core.Internal.Request (addHeader, Request)
import Servant.API.BasicAuth (BasicAuthData(BasicAuthData))

-- | Authenticate a request using Basic Authentication
basicAuthReq :: BasicAuthData -> Request -> Request
basicAuthReq (BasicAuthData user pass) req =
    let authText = decodeUtf8 ("Basic " <> encode (user <> ":" <> pass))
    in addHeader "Authorization" authText req
