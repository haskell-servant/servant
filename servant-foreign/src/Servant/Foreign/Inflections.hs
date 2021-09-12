{-# LANGUAGE OverloadedStrings #-}
module Servant.Foreign.Inflections
  ( concatCase
  , snakeCase
  , camelCase
    -- lenses
  , concatCaseL
  , snakeCaseL
  , camelCaseL
  ) where


import           Control.Lens             hiding
                 (cons)
import qualified Data.Char                as C
import           Data.Text                hiding
                 (map)
import           Prelude                  hiding
                 (head, tail)
import           Servant.Foreign.Internal

-- | Simply concat each part of the FunctionName together.
--
-- @[ "get", "documents", "by", "id" ] → "getdocumentsbyid"@
concatCase :: FunctionName -> Text
concatCase = view concatCaseL

concatCaseL :: Getter FunctionName Text
concatCaseL = _FunctionName . to mconcat

-- | Use the snake_case convention.
-- Each part is separated by a single underscore character.
--
-- @[ "get", "documents", "by", "id" ] → "get_documents_by_id"@
snakeCase :: FunctionName -> Text
snakeCase = view snakeCaseL

snakeCaseL :: Getter FunctionName Text
snakeCaseL = _FunctionName . to (intercalate "_")

-- | Use the camelCase convention.
-- The first part is lower case, every other part starts with an upper case character.
--
-- @[ "get", "documents", "by", "id" ] → "getDocumentsById"@
camelCase :: FunctionName -> Text
camelCase = view camelCaseL

camelCaseL :: Getter FunctionName Text
camelCaseL = _FunctionName . to convert
  where
    convert []     = ""
    convert (p:ps) = mconcat $ p : map capitalize ps
    capitalize ""   = ""
    capitalize name = C.toUpper (head name) `cons` tail name
