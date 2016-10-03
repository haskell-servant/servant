{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | This is a module containing an API with all `Servant.API` combinators. It
-- is used for testing only (in particular, checking that instances exist for
-- the core servant classes for each combinator), and should not be imported.
module Servant.API.Internal.Test.ComprehensiveAPI where

import           Data.Proxy

import           Servant.API

type GET = Get '[JSON] NoContent

type ComprehensiveAPI =
  ComprehensiveAPIWithoutRaw :<|>
  Raw

comprehensiveAPI :: Proxy ComprehensiveAPI
comprehensiveAPI = Proxy

type ComprehensiveAPIWithoutRaw =
  GET :<|>
  Get '[JSON] Int :<|>
  Capture "foo" Int :> GET :<|>
  Header "foo" Int :> GET :<|>
  HttpVersion :> GET :<|>
  IsSecure :> GET :<|>
  QueryParam "foo" Int :> GET :<|>
  QueryParams "foo" Int :> GET :<|>
  QueryFlag "foo" :> GET :<|>
  RemoteHost :> GET :<|>
  ReqBody '[JSON] Int :> GET :<|>
  Get '[JSON] (Headers '[Header "foo" Int] NoContent) :<|>
  "foo" :> GET :<|>
  Vault :> GET :<|>
  Verb 'POST 204 '[JSON] NoContent :<|>
  Verb 'POST 204 '[JSON] Int :<|>
  WithNamedContext "foo" '[] GET :<|>
  CaptureAll "foo" Int :> GET

comprehensiveAPIWithoutRaw :: Proxy ComprehensiveAPIWithoutRaw
comprehensiveAPIWithoutRaw = Proxy
