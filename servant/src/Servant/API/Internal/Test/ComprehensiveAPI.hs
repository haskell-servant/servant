{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Servant.API.Internal.Test.ComprehensiveAPI where

import           Data.Proxy

import           Servant.API

type GET = Get '[JSON] ()

type ComprehensiveAPI =
  GET :<|>
  Get '[JSON] Int :<|>
  Capture "foo" Int :> GET :<|>
  Header "foo" Int :> GET :<|>
  HttpVersion :> GET :<|>
  IsSecure :> GET :<|>
  QueryParam "foo" Int :> GET :<|>
  QueryParams "foo" Int :> GET :<|>
  QueryFlag "foo" :> GET :<|>
-- Raw :<|>
  RemoteHost :> GET :<|>
  ReqBody '[JSON] Int :> GET :<|>
-- Get '[JSON] (Headers '[Header "foo" Int] ()) :<|>
  "foo" :> GET :<|>
  Vault :> GET :<|>
  Verb 'POST 204 '[JSON] () :<|>
  Verb 'POST 204 '[JSON] Int

comprehensiveAPI :: Proxy ComprehensiveAPI
comprehensiveAPI = Proxy
