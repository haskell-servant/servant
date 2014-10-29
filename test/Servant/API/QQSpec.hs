{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE UndecidableInstances  #-}
module Servant.API.QQSpec where

import Test.Hspec

import Servant.API
{-import Servant.API.QQ-}

data HTrue
data HFalse

-- Kiselyov's Type Equality predicate
class  TypeEq x y b | x y -> b where { areEq :: x -> y -> Bool }
instance               TypeEq x x HTrue where { areEq _ _ = True }
instance b ~ HFalse => TypeEq x y b where     { areEq _ _ = False}

type SimpleGet = [sitemap|
GET  hello  ()
|]

type SimpleGet' = "hello" :> Get ()
type SimpleGet'' = "hello" :> Get Bool

spec :: Spec
spec = do
    describe "'sitemap' QuasiQuoter" $ do
        it "Handles simple GET types" $ do
            areEq (undefined::SimpleGet) (undefined::SimpleGet') `shouldBe` True
            areEq (undefined::SimpleGet) (undefined::SimpleGet'') `shouldBe` False
