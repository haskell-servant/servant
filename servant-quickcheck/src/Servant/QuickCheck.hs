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

  -- * Server properties
  -- | Functions to verify that a server meets certain properties.
  --
  -- Example:
  --
  -- > server :: Server API
  -- > server = return . show
  -- >
  -- >
  -- > test :: Spec
  -- > test = describe "my server" $ do
  -- >
  -- >    it "never throws a 500 on valid input" $ do
  -- >      withServantServer api server $ \url ->
  -- >        serverSatisfiers api url emptyPredicates never500s 100
    serverSatisfies

  -- * Server equality
  -- | Functions to verify that two servers behave identically.
  --
  -- This can be useful when for example rewriting or refactoring an
  -- application.
  --
  -- Example:
  --
  -- > server :: Server API
  -- > server = return . show
  -- >
  -- > server2 :: Server API
  -- > server2 = const $ return "hi"
  -- >
  -- > test :: Spec
  -- > test = describe "my new server" $ do
  -- >
  -- >    it "behaves like the old one" $ do
  -- >      withServantServer api server $ \url1 ->
  -- >        withServantServer api server2 $ \url2 ->
  -- >          serversEqual api url1 url2 100
  --
  , serversEqual

  -- * Server benchmarking
  -- | Functions that randomly generate and run benchmarking scripts
  , serverBenchmark
  , BenchOptions(..)
  , defaultBenchOptions


  -- * Test setup helpers
  -- | Helpers to setup and teardown @servant@ servers during tests.
  , withServantServer

  -- * Predicates
  -- | Predicates (functions with signatures @a -> Bool@) are used to filter
  -- out QuickCheck-generated values (so as to specify that requests must
  -- possess certain properties) and to check that the response specifies the
  -- expected properties.
  , Predicates
  , emptyPredicates
  , addPredicate
  , addPolyPredicate

  -- ** Predicate convenience functions
  , addRightPredicate
  , addLeftPredicate

  -- ** Useful predicates
  , never500s
  , onlyJsonObjects

  ) where

import Servant.QuickCheck.Internal
