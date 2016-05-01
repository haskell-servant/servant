-- | @Servant.QuickCheck@ provides utilities related to using QuickCheck over an API.
-- Rather than specifying properties that individual handlers must satisfy,
-- you can state properties that ought to hold true of the entire API.
--
-- While the API must be described with @servant@ types, the server being
-- tested itself need not be implemented with @servant-server@ (or indeed,
-- written in Haskell).
--
-- The documentation of the <#useful-preds Useful predicates> sections is
-- meant to serve as a set of helpful pointers for learning more about best
-- practices concerning REST APIs.
module Servant.QuickCheck
  (

  -- * Property testing
    serverSatisfies
  -- ** Predicates
  -- *** #useful-preds# Useful predicates
  -- | The predicates below are often useful. Some check RFC compliance; some are
  -- best practice, and some are useful to check that APIs follow in-house
  -- best-practices. Included in the documentation for each is a list of
  -- references to any relevant RFCs and other links, as well as what type of
  -- predicate it is (__RFC Compliance__, __Best Practice__, __Optional__).
  --
  -- RFCs distinguish between the force of requirements (e.g. __MUST__ vs.
  -- __SHOULD__). __RFC Compliance__ includes any absolute requirements present
  -- in RFCs. The __Best Practices__ includes, in addition to RFC
  -- recommendations, recommendations found elsewhere or generally accepted.
  , not500
  , onlyJsonObjects
  , notAllowedContainsAllowHeader
  , unauthorizedContainsWWWAuthenticate
  , getsHaveCacheControlHeader
  , headsHaveCacheControlHeader
  , createContainsValidLocation
  -- *** Predicate utilities and types
  , (<%>)
  , Predicates
  , ResponsePredicate(..)
  , RequestPredicate(..)

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

  -- * Test setup helpers
  -- | Helpers to setup and teardown @servant@ servers during tests.
  , withServantServer
  , withServantServerAndContext
  , defaultArgs

  -- ** Re-exports
  , BaseUrl(..)
  , Scheme(..)
  , Args(..)


  ) where

import Servant.QuickCheck.Internal
import Servant.Client (BaseUrl(..), Scheme(..))
import Test.QuickCheck (Args(..), stdArgs)

defaultArgs :: Args
defaultArgs = stdArgs { maxSuccess = 1000 }
