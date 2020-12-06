{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wwarn -Wno-deferred-type-errors #-}
module Servant.Server.Internal.ContextSpec (spec) where

import           Data.Proxy
                 (Proxy (..))
import           Test.Hspec
                 (Spec, context, describe, it, shouldBe)
import           Test.ShouldNotTypecheck
                 (shouldNotTypecheck)

import           Servant.API
import           Servant.Server.Internal.Context

spec :: Spec
spec = do
  describe "getContextEntry" $ do
    it "gets the context if a matching one exists" $ do
      let cxt = 'a' :. EmptyContext
      getContextEntry cxt `shouldBe` 'a'

    it "gets the first matching context" $ do
      let cxt = 'a' :. 'b' :. EmptyContext
      getContextEntry cxt `shouldBe` 'a'

    it "does not typecheck if type does not exist" $ do
      let cxt = 'a' :. EmptyContext
          x = getContextEntry cxt :: Bool
      shouldNotTypecheck x

    context "Show instance" $ do
      it "has a Show instance" $ do
        let cxt = 'a' :. True :. EmptyContext
        show cxt `shouldBe` "'a' :. True :. EmptyContext"

      context "bracketing" $ do
        it "works" $ do
          let cxt = 'a' :. True :. EmptyContext
          show (Just cxt) `shouldBe` "Just ('a' :. True :. EmptyContext)"

        it "works with operators" $ do
          let cxt = (1 :: Integer) :. 'a' :. EmptyContext :<|> 'b' :. True :. EmptyContext
          show cxt `shouldBe` "1 :. 'a' :. EmptyContext :<|> 'b' :. True :. EmptyContext"

  describe "descendIntoNamedContext" $ do
    let cxt :: Context [Char, NamedContext "sub" '[Char]]
        cxt =
          'a' :.
          (NamedContext subContext :: NamedContext "sub" '[Char])
          :. EmptyContext
        subContext = 'b' :. EmptyContext
    it "allows extracting subcontexts" $ do
      descendIntoNamedContext (Proxy :: Proxy "sub") cxt `shouldBe` subContext

    it "allows extracting entries from subcontexts" $ do
      getContextEntry (descendIntoNamedContext (Proxy :: Proxy "sub") cxt :: Context '[Char])
        `shouldBe` 'b'

    it "does not typecheck if subContext has the wrong type" $ do
      let x = descendIntoNamedContext (Proxy :: Proxy "sub") cxt :: Context '[Int]
      shouldNotTypecheck (show x)

    it "does not typecheck if subContext with that name doesn't exist" $ do
      let x = descendIntoNamedContext (Proxy :: Proxy "foo") cxt :: Context '[Char]
      shouldNotTypecheck (show x)
