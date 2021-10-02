{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Basic Authentication for clients
module Servant.Client.Core.BasicAuth (
    basicAuthReq,
    ) where

import           Data.ByteString.Base64
                 (encode)
import           Data.Text.Encoding
                 (decodeUtf8)
import           Servant.API.BasicAuth
                 (BasicAuthData (BasicAuthData))
import           Servant.Client.Core.Request
                 (Request, addHeader)

-- | Authenticate a request using Basic Authentication
basicAuthReq :: BasicAuthData -> Request -> Request
basicAuthReq (BasicAuthData user pass) req =
    let authText = decodeUtf8 ("Basic " <> encode (user <> ":" <> pass))
    in addHeader "Authorization" authText req
