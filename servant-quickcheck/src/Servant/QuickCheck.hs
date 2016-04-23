-- | @Servant.QuickCheck@ provides utilities related to using QuickCheck over an API.
-- Rather than specifying properties that individual handlers must satisfy,
-- you can state properties that ought to hold true of the entire API.
--
-- While the API must be described with @servant@ types, the server being
-- tested itself need not be implemented with @servant-server@ (or indeed,
-- written in Haskell).
--
-- /N.B./ The examples given here assume the following setup:
--
-- > import Servant
-- > import Servant.QuickCheck
-- > import Test.Hspec
-- >
-- > type API = ReqBody '[JSON] Int :> Post '[JSON] String
-- >
-- > api :: Proxy API
-- > api = Proxy
module Servant.QuickCheck
  (

   serversEqual
  -- * Test setup helpers
  -- | Helpers to setup and teardown @servant@ servers during tests.
  , withServantServer

  -- * Response equality
  , bodyEquality
  , allEquality
  , ResponseEquality(getResponseEquality)

  -- ** Re-exports
  , BaseUrl(..)
  , Scheme(..)


  ) where

import Servant.QuickCheck.Internal
import Servant.Client (BaseUrl(..), Scheme(..))
