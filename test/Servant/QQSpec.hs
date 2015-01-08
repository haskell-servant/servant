{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE UndecidableInstances  #-}
module Servant.QQSpec where

import Test.Hspec ( Expectation, Spec, shouldBe, it, describe, pendingWith )

spec = describe "this" $ it "is" $  pendingWith "playing around"
{-
import Servant.API
    ( (:<|>),
      ReqBody,
      QueryParam,
      MatrixParam,
      Put,
      Get,
      Post,
      Capture,
      (:>),
      JSON,
      sitemap )

--------------------------------------------------------------------------
-- Types for testing
--------------------------------------------------------------------------

-- Methods ---------------------------------------------------------------
type SimpleGet = [sitemap|
GET  hello  ()
|]
type SimpleGet' = "hello" :> Get '[JSON] ()
type SimpleGet'' = "hello" :> Get '[JSON] Bool

type SimpleGet2 = [sitemap|
GET  hello  Bool
|]
type SimpleGet2' = "hello" :> Get '[JSON] Bool
type SimpleGet2'' = "hello" :> Get '[JSON] Int

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

type SimplePut = [sitemap|
PUT  hello  ()
|]
type SimplePut' = "hello" :> Put ()
type SimplePut'' = "hello" :> Put Bool

type SimplePut2 = [sitemap|
PUT  hello  Bool
|]
type SimplePut2' = "hello" :> Put Bool
type SimplePut2'' = "hello" :> Put ()

-- Parameters ------------------------------------------------------------

type SimpleReqBody = [sitemap|
POST  hello  () -> Bool
|]
type SimpleReqBody' = "hello" :> ReqBody () :> Post Bool
type SimpleReqBody'' = "hello" :> ReqBody Bool :> Post ()

type SimpleCapture = [sitemap|
POST  hello/p:Int   Bool
|]
type SimpleCapture' = "hello" :> Capture "p" Int :> Post Bool
type SimpleCapture'' = "hello" :> Capture "r" Int :> Post Bool
type SimpleCapture''' = "hello" :> Capture "p" Bool :> Post Bool

type SimpleQueryParam = [sitemap|
POST  hello/?p:Int   Bool
|]
type SimpleQueryParam' = "hello" :> QueryParam "p" Int :> Post Bool
type SimpleQueryParam'' = "hello" :> QueryParam "r" Int :> Post Bool
type SimpleQueryParam''' = "hello" :> QueryParam "p" Bool :> Post Bool

type SimpleMatrixParam = [sitemap|
POST  hello;p:Int   Bool
|]
type SimpleMatrixParam' = "hello" :> MatrixParam "p" Int :> Post Bool
type SimpleMatrixParam'' = "hello" :> MatrixParam "r" Int :> Post Bool
type SimpleMatrixParam''' = "hello" :> MatrixParam "p" Bool :> Post Bool

type ComplexMatrixParam = [sitemap|
POST  hello;p:Int;q:String/world;r:Int   Bool
|]
type ComplexMatrixParam' = "hello" :> MatrixParam "p" Int :> MatrixParam "q" String :> "world" :> MatrixParam "r" Int :> Post Bool
type ComplexMatrixParam'' = "hello" :> MatrixParam "p" Int :> MatrixParam "q" String :> "world" :> MatrixParam "s" Int :> Post Bool
type ComplexMatrixParam''' = "hello" :> MatrixParam "p" Int :> MatrixParam "q" String :> "world" :> MatrixParam "r" Bool :> Post Bool

-- Combinations ----------------------------------------------------------

type TwoPaths = [sitemap|
POST hello  Bool
GET  hello  Bool
|]
type TwoPaths' = ("hello" :> Post Bool) :<|> ("hello" :> Get '[JSON] Bool)

type WithInlineComments = [sitemap|
GET  hello  Bool   -- This is a comment
|]
type WithInlineComments' = "hello" :> Get '[JSON] Bool

type WithInlineComments2 = [sitemap|
GET  hello  Bool
-- This is a comment
|]
type WithInlineComments2' = "hello" :> Get '[JSON] Bool


type WithBlockComments = [sitemap|
GET  hello  Bool   {-
POST hello  Bool
-}
|]
type WithBlockComments' = "hello" :> Get '[JSON] Bool

type WithBlockComments2 = [sitemap|
GET  hello  Bool   {-
POST hello  Bool
-}
POST hello Bool
|]
type WithBlockComments2' = ("hello" :> Get '[JSON] Bool) :<|> ("hello" :> Post Bool)

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
        it "Handles simple PUT types" $ do
            (u::SimplePut)  ~= (u::SimplePut'  ) ~> True
            (u::SimplePut)  ~= (u::SimplePut'' ) ~> False
            (u::SimplePut2) ~= (u::SimplePut2' ) ~> True
            (u::SimplePut2) ~= (u::SimplePut2'') ~> False
        it "Handles simple request body types" $ do
            (u::SimpleReqBody) ~= (u::SimpleReqBody' ) ~> True
            (u::SimpleReqBody) ~= (u::SimpleReqBody'') ~> False
        it "Handles simple captures" $ do
            (u::SimpleCapture) ~= (u::SimpleCapture' ) ~> True
            (u::SimpleCapture) ~= (u::SimpleCapture'') ~> False
            (u::SimpleCapture) ~= (u::SimpleCapture''') ~> False
        it "Handles simple querystring parameters" $ do
            (u::SimpleQueryParam) ~= (u::SimpleQueryParam' ) ~> True
            (u::SimpleQueryParam) ~= (u::SimpleQueryParam'') ~> False
            (u::SimpleQueryParam) ~= (u::SimpleQueryParam''') ~> False
        it "Handles simple matrix parameters" $ do
            (u::SimpleMatrixParam) ~= (u::SimpleMatrixParam' ) ~> True
            (u::SimpleMatrixParam) ~= (u::SimpleMatrixParam'') ~> False
            (u::SimpleMatrixParam) ~= (u::SimpleMatrixParam''') ~> False
        it "Handles more complex matrix parameters" $ do
            (u::ComplexMatrixParam) ~= (u::ComplexMatrixParam' ) ~> True
            (u::ComplexMatrixParam) ~= (u::ComplexMatrixParam'') ~> False
            (u::ComplexMatrixParam) ~= (u::ComplexMatrixParam''') ~> False
        it "Handles multiples paths" $ do
            (u::TwoPaths) ~= (u::TwoPaths') ~> True
        it "Ignores inline comments" $ do
            (u::WithInlineComments) ~= (u::WithInlineComments') ~> True
            (u::WithInlineComments2) ~= (u::WithInlineComments2') ~> True
        it "Ignores inline comments" $ do
            (u::WithBlockComments) ~= (u::WithBlockComments') ~> True
            (u::WithBlockComments2) ~= (u::WithBlockComments2') ~> True

-}

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
(~>) :: (Show a, Eq a) => a -> a -> Expectation
(~>) = shouldBe
