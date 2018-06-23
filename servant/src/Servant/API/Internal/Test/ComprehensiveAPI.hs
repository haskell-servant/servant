{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | This is a module containing an API with all `Servant.API` combinators. It
-- is used for testing only (in particular, checking that instances exist for
-- the core servant classes for each combinator), and should not be imported.
module Servant.API.Internal.Test.ComprehensiveAPI where

import           Data.Proxy
import           Servant.API

type GET = Verb' 'GET (NoContent 200)

type ComprehensiveAPI =
  ComprehensiveAPIWithoutRaw :<|>
  Raw

comprehensiveAPI :: Proxy ComprehensiveAPI
comprehensiveAPI = Proxy

type ComprehensiveAPIWithoutRaw =
  GET :<|>
  Get '[JSON] Int :<|>
  Capture' '[Description "example description"] "foo" Int :> GET :<|>
  Header "foo" Int :> GET :<|>
  Header' '[Required, Lenient] "bar" Int :> GET :<|>
  HttpVersion :> GET :<|>
  IsSecure :> GET :<|>
  QueryParam "foo" Int :> GET :<|>
  QueryParam' '[Required, Lenient] "bar" Int :> GET :<|>
  QueryParams "foo" Int :> GET :<|>
  QueryFlag "foo" :> GET :<|>
  RemoteHost :> GET :<|>
  ReqBody '[JSON] Int :> GET :<|>
  ReqBody' '[Lenient] '[JSON] Int :> GET :<|>
  Verb' 'GET (Headers '[Header "foo" Int] :> NoContent 200) :<|>
  "foo" :> GET :<|>
  Vault :> GET :<|>
  PostNoContent :<|>
  Verb 'POST 204 '[JSON] Int :<|>
  WithNamedContext "foo" '[] GET :<|>
  CaptureAll "foo" Int :> GET :<|>
  Summary "foo" :> GET :<|>
  Description "foo" :> GET :<|>
  EmptyAPI

comprehensiveAPIWithoutRaw :: Proxy ComprehensiveAPIWithoutRaw
comprehensiveAPIWithoutRaw = Proxy
