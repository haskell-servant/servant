{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | This is a module containing an API with all `Servant.API` combinators. It
-- is used for testing only (in particular, checking that instances exist for
-- the core servant classes for each combinator).
module Servant.Test.ComprehensiveAPI where

import           Data.Proxy
                 (Proxy (..))
import           Servant.API
import           Servant.Types.SourceT
                 (SourceT)

type GET = Get '[JSON] NoContent

type ComprehensiveAPI =
  ComprehensiveAPIWithoutRaw :<|>
  "raw" :> Raw

comprehensiveAPI :: Proxy ComprehensiveAPI
comprehensiveAPI = Proxy

type ComprehensiveAPIWithoutRaw =
  GET :<|>
  "get-int"          :> Get '[JSON] Int :<|>
  "capture"          :> Capture' '[Description "example description"] "foo" Int :> GET :<|>
  "header"           :> Header "foo" Int :> GET :<|>
  "header-lenient"   :> Header' '[Required, Lenient] "bar" Int :> GET :<|>
  "http-version"     :> HttpVersion :> GET :<|>
  "is-secure"        :> IsSecure :> GET :<|>
  "param"            :> QueryParam "foo" Int :> GET :<|>
  "param-lenient"    :>  QueryParam' '[Required, Lenient] "bar" Int :> GET :<|>
  "params"           :> QueryParams "foo" Int :> GET :<|>
  "flag"             :> QueryFlag "foo" :> GET :<|>
  "remote-host"      :> RemoteHost :> GET :<|>
  "req-body"         :> ReqBody '[JSON] Int :> GET :<|>
  "req-body-lenient" :> ReqBody' '[Lenient] '[JSON] Int :> GET :<|>
  "res-headers"      :> Get '[JSON] (Headers '[Header "foo" Int] NoContent) :<|>
  "foo"              :> GET :<|>
  "vault"            :> Vault :> GET :<|>
  "post-no-content"  :> Verb 'POST 204 '[JSON] NoContent :<|>
  "post-int"         :> Verb 'POST 204 '[JSON] Int :<|>
  "streaming"        :> StreamBody NetstringFraming JSON (SourceT IO Int) :> Stream 'GET 200 NetstringFraming JSON (SourceT IO Int) :<|>
  "named-context"    :> WithNamedContext "foo" '[] GET :<|>
  "capture-all"      :>  CaptureAll "foo" Int :> GET :<|>
  "summary"          :> Summary "foo" :> GET :<|>
  "description"      :> Description "foo" :> GET :<|>
  "empty-api"        :> EmptyAPI

comprehensiveAPIWithoutRaw :: Proxy ComprehensiveAPIWithoutRaw
comprehensiveAPIWithoutRaw = Proxy
