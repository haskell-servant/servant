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

--------------------------------------------------------------------------
-- Types for testing
--------------------------------------------------------------------------


type SimpleGet = [sitemap|
GET  hello  ()
|]
type SimpleGet' = "hello" :> Get ()
type SimpleGet'' = "hello" :> Get Bool

type SimpleGet2 = [sitemap|
GET  hello  Bool
|]
type SimpleGet2' = "hello" :> Get Bool
type SimpleGet2'' = "hello" :> Get Int

type SimplePost = [sitemap|
POST  hello  ()
|]
type SimplePost' = "hello" :> Post ()
type SimplePost'' = "hello" :> Post Bool

type SimplePost2 = [sitemap|
POST  hello  Bool
|]
type SimplePost2' = "hello" :> Post Bool
type SimplePost2'' = "hello" :> Post ()

type SimpleReqBody = [sitemap|
POST  hello  () -> Bool
|]
type SimpleReqBody' = "hello" :> ReqBody () :> Post Bool
type SimpleReqBody'' = "hello" :> ReqBody Bool :> Post ()

--------------------------------------------------------------------------
-- Spec
--------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "'sitemap' QuasiQuoter" $ do
        it "Handles simple GET types" $ do
            (u::SimpleGet)  ~= (u::SimpleGet'  ) ~> True
            (u::SimpleGet)  ~= (u::SimpleGet'' ) ~> False
            (u::SimpleGet2) ~= (u::SimpleGet2' ) ~> True
            (u::SimpleGet2) ~= (u::SimpleGet2'') ~> False
        it "Handles simple POST types" $ do
            (u::SimplePost)  ~= (u::SimplePost'  ) ~> True
            (u::SimplePost)  ~= (u::SimplePost'' ) ~> False
            (u::SimplePost2) ~= (u::SimplePost2' ) ~> True
            (u::SimplePost2) ~= (u::SimplePost2'') ~> False
        it "Handles simple request body types" $ do
            (u::SimpleReqBody) ~= (u::SimpleReqBody' ) ~> True
            (u::SimpleReqBody) ~= (u::SimpleReqBody'') ~> False


--------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------
data HTrue
data HFalse

-- Kiselyov's Type Equality predicate
class  TypeEq x y b | x y -> b where { areEq :: x -> y -> Bool }
instance               TypeEq x x HTrue where { areEq _ _ = True }
instance b ~ HFalse => TypeEq x y b where     { areEq _ _ = False}

infix 4 ~=
(~=) :: TypeEq x y b => x -> y -> Bool
(~=) = areEq

u :: a
u = undefined

infix 3 ~>
(~>) = shouldBe
