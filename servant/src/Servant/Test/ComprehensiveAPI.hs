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
    ComprehensiveAPIWithoutStreamingOrRaw'
    (EmptyEndpoint :<|> StreamingEndpoint :<|> RawEndpoint)

type RawEndpoint =
    "raw" :> Raw

type StreamingEndpoint =
    "streaming" :> StreamBody' '[Description "netstring"] NetstringFraming JSON (SourceT IO Int) :> Stream 'GET 200 NetstringFraming JSON (SourceT IO Int)

type EmptyEndpoint =
    "empty-api" :> EmptyAPI

comprehensiveAPI :: Proxy ComprehensiveAPI
comprehensiveAPI = Proxy

type ComprehensiveAPIWithoutRaw =
    ComprehensiveAPIWithoutStreamingOrRaw'
    (EmptyEndpoint :<|> StreamingEndpoint)

comprehensiveAPIWithoutRaw :: Proxy ComprehensiveAPIWithoutRaw
comprehensiveAPIWithoutRaw = Proxy

type ComprehensiveAPIWithoutStreaming =
    ComprehensiveAPIWithoutStreamingOrRaw'
    (EmptyEndpoint :<|> RawEndpoint)

comprehensiveAPIWithoutStreaming :: Proxy ComprehensiveAPIWithoutStreaming
comprehensiveAPIWithoutStreaming = Proxy

-- | @:: API -> API@, so we have linear structure of the API.
type ComprehensiveAPIWithoutStreamingOrRaw' endpoint =
    GET
    :<|> "get-int"          :> Get '[JSON] Int
    :<|> "capture"          :> Capture' '[Description "example description"] "bar" Int :> GET
    :<|> "capture-lenient"  :> Capture' '[Lenient] "foo" Int :> GET
    :<|> "header"           :> Header "foo" Int :> GET
    :<|> "header-lenient"   :> Header' '[Required, Lenient] "bar" Int :> GET
    :<|> "http-version"     :> HttpVersion :> GET
    :<|> "is-secure"        :> IsSecure :> GET
    :<|> "param"            :> QueryParam "foo" Int :> GET
    :<|> "param-lenient"    :> QueryParam' '[Required, Lenient] "bar" Int :> GET
    :<|> "params"           :> QueryParams "foo" Int :> GET
    :<|> "flag"             :> QueryFlag "foo" :> GET
    :<|> "remote-host"      :> RemoteHost :> GET
    :<|> "req-body"         :> ReqBody '[JSON] Int :> GET
    :<|> "req-body-lenient" :> ReqBody' '[Lenient] '[JSON] Int :> GET
    :<|> "res-headers"      :> Get '[JSON] (Headers '[Header "foo" Int] NoContent)
    :<|> "foo"              :> GET
    :<|> "vault"            :> Vault :> GET
    :<|> "post-no-content"  :> PostNoContent
    :<|> "post-int"         :> Verb 'POST 204 '[JSON] Int
    :<|> "named-context"    :> WithNamedContext "foo" '[] GET
    :<|> "capture-all"      :> CaptureAll "foo" Int :> GET
    :<|> "summary"          :> Summary "foo" :> GET
    :<|> "description"      :> Description "foo" :> GET
    :<|> "alternative"      :> ("left" :> GET :<|> "right" :> GET)
    :<|> "fragment"         :> Fragment Int :> GET
    :<|> endpoint

type ComprehensiveAPIWithoutStreamingOrRaw = ComprehensiveAPIWithoutStreamingOrRaw' EmptyEndpoint

comprehensiveAPIWithoutStreamingOrRaw :: Proxy ComprehensiveAPIWithoutStreamingOrRaw
comprehensiveAPIWithoutStreamingOrRaw = Proxy
