{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | This is a module containing an API with all `Servant.API` combinators. It
-- is used for testing only (in particular, checking that instances exist for
-- the core servant classes for each combinator), and should not be imported.
module Servant.API.Internal.Test.ComprehensiveAPI (
    ComprehensiveAPI,
    comprehensiveAPI,
    ComprehensiveAPIWithoutRaw,
    comprehensiveAPIWithoutRaw,
  ) where

import           Data.ByteString
                 (ByteString)
import           Data.Proxy
                 (Proxy (..))
import           Servant.API

type GET = VerbNoContent 'GET (NoContent 200)

-- | API using most built-in combinators and 'Raw'.
type ComprehensiveAPI = ComprehensiveAPIWithoutRaw :<|> Raw

comprehensiveAPI :: Proxy ComprehensiveAPI
comprehensiveAPI = Proxy

-- | API using most built-in combinators, but not 'Raw'.
type ComprehensiveAPIWithoutRaw = GET
   :<|> Get '[JSON] Int
   :<|> Capture' '[Description "example description"] "foo" Int :> GET
   :<|> Header "foo" Int :> GET
   :<|> Header' '[Required, Lenient] "bar" Int :> GET
   :<|> HttpVersion :> GET
   :<|> IsSecure :> GET
   :<|> QueryParam "foo" Int :> GET
   :<|> QueryParam' '[Required, Lenient] "bar" Int :> GET
   :<|> QueryParams "foo" Int :> GET
   :<|> QueryFlag "foo" :> GET
   :<|> RemoteHost :> GET
   :<|> ReqBody '[JSON] Int :> GET
   :<|> ReqBody' '[Lenient] '[JSON] Int :> GET
   :<|> VerbNoContent 'GET (Headers '[Header "foo" Int] :> NoContent 200)
   :<|> "foo" :> GET
   :<|> Vault :> GET
   :<|> PostNoContent
   :<|> Verb 'POST '[JSON] (Result 204 Int)
   :<|> StreamGet 200 '[OctetStream] NoFraming (StreamGenerator ByteString)
   :<|> WithNamedContext "foo" '[] GET
   :<|> CaptureAll "foo" Int :> GET
   :<|> Summary "foo" :> GET
   :<|> Description "foo" :> GET
   :<|> EmptyAPI

comprehensiveAPIWithoutRaw :: Proxy ComprehensiveAPIWithoutRaw
comprehensiveAPIWithoutRaw = Proxy
