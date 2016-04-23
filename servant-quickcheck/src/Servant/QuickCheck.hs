-- | @Servant.QuickCheck@ provides utilities related to using QuickCheck over an API.
-- Rather than specifying properties that individual handlers must satisfy,
-- you can state properties that ought to hold true of the entire API.
--
-- While the API must be described with @servant@ types, the server being
-- tested itself need not be implemented with @servant-server@ (or indeed,
-- written in Haskell).
--
module Servant.QuickCheck
  (


  -- * Test setup helpers
  -- | Helpers to setup and teardown @servant@ servers during tests.
    withServantServer


  -- * Equality testing
  , serversEqual
  -- ** Response equality
  -- | Often the normal equality of responses is not what we want. For example,
  -- if responses contain a @Date@ header with the time of the response,
  -- responses will fail to be equal even though they morally are. This datatype
  -- represents other means of checking equality
  -- *** Useful @ResponseEquality@s
  , bodyEquality
  , allEquality
  -- ** Response equality type
  , ResponseEquality(..)

  -- * Property testing
  , serverSatisfies
  -- ** Predicates
  -- *** Useful predicates
  , not500
  , onlyJsonObjects
  , notAllowedContainsAllowHeader
  , unauthorizedContainsWWWAuthenticate
  -- *** Predicate utilities and types
  , (<%>)
  , Predicates
  , ResponsePredicate(..)
  , RequestPredicate(..)

  -- ** Re-exports
  , BaseUrl(..)
  , Scheme(..)


  ) where

import Servant.QuickCheck.Internal
import Servant.Client (BaseUrl(..), Scheme(..))
