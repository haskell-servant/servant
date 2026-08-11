{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Servant.API.VerbsSpec where

import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (Refl))
import Test.Hspec

import Servant.API
import Servant.API.MultiVerb (MultiVerb, Respond)

spec :: Spec
spec = describe "Servant.API.Verbs" $ do
  describe "QUERY" $ do
    it "reflects the upstream QUERY method" $
      reflectMethod (Proxy :: Proxy 'QUERY) `shouldBe` "QUERY"

    it "has the expected convenience aliases and statuses" $ do
      expectTypeEquality (Refl :: Query '[JSON] Int :~: Verb 'QUERY 200 '[JSON] Int)
      expectTypeEquality (Refl :: QueryAccepted '[JSON] Int :~: Verb 'QUERY 202 '[JSON] Int)
      expectTypeEquality (Refl :: QueryNonAuthoritative '[JSON] Int :~: Verb 'QUERY 203 '[JSON] Int)
      expectTypeEquality (Refl :: QueryNoContent :~: NoContentVerb 'QUERY)
      expectTypeEquality (Refl :: QueryResetContent '[JSON] Int :~: Verb 'QUERY 205 '[JSON] Int)
      expectTypeEquality (Refl :: QueryPartialContent '[JSON] Int :~: Verb 'QUERY 206 '[JSON] Int)

    it "can instantiate generic endpoint combinators" $
      queryGenericCombinators

expectTypeEquality :: a :~: b -> Expectation
expectTypeEquality Refl = pure ()

queryGenericCombinators :: Expectation
queryGenericCombinators =
  let uverb = Proxy :: Proxy (UVerb 'QUERY '[JSON] '[WithStatus 200 Int])
      multiVerb = Proxy :: Proxy (MultiVerb 'QUERY '[JSON] '[Respond 200 "OK" Int] Int)
      stream = Proxy :: Proxy (Stream 'QUERY 200 NoFraming JSON Int)
      noContentVerb = Proxy :: Proxy (NoContentVerb 'QUERY)
   in uverb `seq` multiVerb `seq` stream `seq` noContentVerb `seq` pure ()
