{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- This module uses ideas from the Chapter 10 of the book @Thinking with Types@ by Sandy Maguire.
-- 
-- That chapter is based on the work of Xia Li-yao, namely the [first-class-families](https://github.com/Lysxia/first-class-families) library.
-- 
module Servant.Symbols (
  Modify,
  Symbol,
  Exp,
  ToList,
  FromList,
  ToLower,
  ToUpper,
  DropWhile,
  DropWhile1,
  DropWhileNot,
  DropWhileNot1,
  CheckSymbolHasSingleCharacter,
  CheckListHasSingleSymbol
) where

import Data.Kind (Type)
import Data.Symbol.Ascii (ToList, ToLower, ToUpper)
import GHC.Base (Symbol)
import GHC.TypeLits (ErrorMessage (..), TypeError, AppendSymbol)

-- | Kind of type-level expressions indexed by their result type.
type Exp a = a -> Type

-- | Select the function that will modify a 'Symbol' lifted to 'Exp'.
-- 
-- Modules that use the 'Modify' type family (e.g., 'Servant.Record') must be imported together with a module that exports instances of 'Modify'.
-- 
-- The GHC documentation states that @Family instances are implicitly exported, just like class instances@.
-- 
-- Here's an example of a 'Symbol' modification via 'Modify'.
-- >>> type family Modifier (sym :: Symbol) :: Symbol where Modifier sym = DropWhile "_" (DropWhileNot "_" (DropWhile "_" sym))
-- >>> type instance Modify (f sym) = Modifier sym
-- >>> data ExampleMkExp :: Symbol -> Exp Symbol
-- >>> :kind! Modify (ExampleMkExp "_hello_world")
-- Modify (ExampleMkExp "_hello_world") :: Symbol
-- = "world"
type family Modify (a :: Exp Symbol) :: Symbol

-- | Convert a list of 'Symbol's to a 'Symbol'.
-- 
-- Works for ASCII-only 'Symbol's.
type family FromList (syms :: [Symbol]) :: Symbol where
  FromList xs = FromList1 (ToList (FromList1 xs))

type family FromList1 (syms :: [Symbol]) :: Symbol where
  FromList1 '[] = ""
  FromList1 (x : xs) = AppendSymbol x (FromList1 xs)

-- | Assuming @dropSym@ contains a single character, drop that character from the prefix of @sym@.
--
-- >>> :kind! DropWhile "a" "aaaargh"
-- DropWhile "a" "aaaargh" :: Symbol
-- = "rgh"
--
-- >>> :kind! DropWhile "aa" "aaaargh"
-- DropWhile "aa" "aaaargh" :: Symbol
-- = FromList1
--     (ToList1
--        (FromList1
--           (DropWhile1
--              (FromList1 (ToList1 (FromList1 (TypeError ...)) ""))
--              '["a", "a", "a", "a", "r", "g", "h"]))
--        "")
type family DropWhile (dropSym :: Symbol) (sym :: Symbol) :: Symbol where
  DropWhile dropSym sym = FromList (DropWhile1 (CheckSymbolHasSingleCharacter dropSym) (ToList sym))

-- | Check that a 'Symbol' contains a single character.
type family CheckSymbolHasSingleCharacter (sym :: Symbol) :: Symbol where
  CheckSymbolHasSingleCharacter sym = FromList (CheckListHasSingleSymbol (ToList sym))

-- | Check that a '[Symbol]' contains a single 'Symbol'.
--
-- >>> :kind! CheckListHasSingleSymbol '["a"]
-- CheckListHasSingleSymbol '["a"] :: [Symbol]
-- = '["a"]
--
-- >>> :kind! CheckListHasSingleSymbol '["a", "a"]
-- CheckListHasSingleSymbol '["a", "a"] :: [Symbol]
-- = (TypeError ...)
--
-- >>> :kind! CheckListHasSingleSymbol '[]
-- CheckListHasSingleSymbol '[] :: [Symbol]
-- = (TypeError ...)
type family CheckListHasSingleSymbol (syms :: [Symbol]) :: [Symbol] where
  CheckListHasSingleSymbol (x : y : xs) =
    TypeError
      ( 'Text "The Symbol to drop has more than 1 character. The Symbol starts with '"
          :<>: ShowType x
          :<>: 'Text ":"
          :<>: ShowType x
          :$$: 'Text ":...'. Please, fix the error."
      )
  CheckListHasSingleSymbol '[] = TypeError ('Text "The Symbol to drop has no characters. Please, fix the error.")
  CheckListHasSingleSymbol xs = xs

-- | Drop all @dropSym@s from the prefix of @syms@.
--
-- >>> :kind! DropWhile1 "aa" ["aa", "aa", "r", "g", "h"]
-- DropWhile1 "aa" ["aa", "aa", "r", "g", "h"] :: [Symbol]
-- = '["r", "g", "h"]
type family DropWhile1 (dropSym :: Symbol) (syms :: [Symbol]) :: [Symbol] where
  DropWhile1 dropSym (dropSym : xs) = DropWhile1 dropSym xs
  DropWhile1 dropSym xs = xs

-- | Assuming @dropSym@ contains a single character, drop any characters but that character from the prefix of @sym@.
--
-- >>> :kind! DropWhileNot "a" "hgraaaa"
-- DropWhileNot "a" "hgraaaa" :: Symbol
-- = "aaaa"
--
-- >>> :kind! DropWhileNot "aa" "hgraaaa"
-- DropWhileNot "aa" "hgraaaa" :: Symbol
-- = FromList1
--     (ToList1
--        (FromList1
--           (DropWhileNot1
--              (FromList1 (ToList1 (FromList1 (TypeError ...)) ""))
--              '["h", "g", "r", "a", "a", "a", "a"]))
--        "")
type family DropWhileNot (dropSym :: Symbol) (sym :: Symbol) :: Symbol where
  DropWhileNot dropSym sym = FromList (DropWhileNot1 (CheckSymbolHasSingleCharacter dropSym) (ToList sym))

-- | Drop all 'Symbol's but @dropSym@ from the prefix of @syms@.
--
-- >>> :kind! DropWhileNot1 "aa" ["h", "g", "r", "aa", "aa"]
-- DropWhileNot1 "aa" ["h", "g", "r", "aa", "aa"] :: [Symbol]
-- = '["aa", "aa"]
type family DropWhileNot1 (dropSym :: Symbol) (syms :: [Symbol]) :: [Symbol] where
  DropWhileNot1 dropSym (dropSym : xs) = (dropSym ': xs)
  DropWhileNot1 dropSym (x : xs) = DropWhileNot1 dropSym xs
