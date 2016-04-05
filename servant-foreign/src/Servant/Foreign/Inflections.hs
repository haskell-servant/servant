module Servant.Foreign.Inflections
  ( concatCase
  , snakeCase
  , camelCase
    -- lenses
  , concatCaseL
  , snakeCaseL
  , camelCaseL
  ) where


import           Control.Lens hiding (cons)
import qualified Data.Char as C
import           Data.Monoid
import           Data.Text hiding (map)
import           Prelude hiding (head, tail)
import           Servant.Foreign.Internal

concatCaseL :: Getter FunctionName Text
concatCaseL = _FunctionName . to mconcat

-- | Function name builder that simply concat each part together
concatCase :: FunctionName -> Text
concatCase = view concatCaseL

snakeCaseL :: Getter FunctionName Text
snakeCaseL = _FunctionName . to (intercalate "_")

-- | Function name builder using the snake_case convention.
-- each part is separated by a single underscore character.
snakeCase :: FunctionName -> Text
snakeCase = view snakeCaseL

camelCaseL :: Getter FunctionName Text
camelCaseL = _FunctionName . to (convert . map (replace "-" ""))
  where
    convert []     = ""
    convert (p:ps) = mconcat $ p : map capitalize ps
    capitalize ""   = ""
    capitalize name = C.toUpper (head name) `cons` tail name

-- | Function name builder using the CamelCase convention.
-- each part begins with an upper case character.
camelCase :: FunctionName -> Text
camelCase = view camelCaseL
