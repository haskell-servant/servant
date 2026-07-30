{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_HADDOCK not-home #-}

module Servant.API.QueryString (QueryString, DecodedQuery, DeepQuery, FromDeepQuery (..), ToDeepQuery (..), generateDeepParam) where

import Data.Bifunctor (Bifunctor (first))
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import GHC.TypeLits (Symbol)
import Network.HTTP.Types (Query)
import Web.HttpApiData (FromHttpApiData)
import Web.Internal.HttpApiData (FromHttpApiData (..))

-- | Work with the whole query string. This is useful for query strings containing
-- dynamic parameter names. For query strings with static parameter names,
-- 'QueryParam' is more suitable.
--
-- On the server, handlers receive a 'DecodedQuery': WAI has already percent-decoded
-- both names and values (and decoded @+@ as a space). Do not URL-decode them again.
--
-- Clients accept a @PartialEscapeQuery@, whose @EscapeItem@ values explicitly state
-- whether each section should be URL-encoded.
--
-- Example:
--
-- >>> -- /books?author=<author name>&year=<book year>
-- >>> type MyApi = "books" :> QueryString :> Get '[JSON] [Book]
data QueryString
  deriving (Typeable)

-- | A query string after WAI has URL-decoded every parameter name and value.
--
-- This is a documented alias for 'Query', used by server-side 'QueryString'
-- handlers to make the decoding state explicit.
type DecodedQuery = Query

-- | Extract an deep object from a query string.
--
-- Example:
--
-- >>> -- /books?filter[author][name]=<author name>&filter[year]=<book year>
-- >>> type MyApi = "books" :> DeepQuery "filter" BookQuery :> Get '[JSON] [Book]
data DeepQuery (sym :: Symbol) (a :: Type)
  deriving (Typeable)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> data BookQuery
-- >>> instance ToJSON Book where { toJSON = undefined }

-- | Extract a deep object from (possibly nested) query parameters.
-- a param like @filter[a][b][c]=d@ will be represented as
-- @'(["a", "b", "c"], Just "d")'@. Note that a parameter with no
-- nested field is possible: @filter=a@ will be represented as
-- @'([], Just "a")'@. Parameter names and values are URL-decoded before this
-- function is called.
class FromDeepQuery a where
  fromDeepQuery :: [([Text], Maybe Text)] -> Either String a

instance FromHttpApiData a => FromDeepQuery (Map Text a) where
  fromDeepQuery params =
    let parseParam ([k], Just rawV) = (k,) <$> first T.unpack (parseQueryParam rawV)
        parseParam (_, Nothing) = Left "Empty map value"
        parseParam ([], _) = Left "Empty map parameter"
        parseParam (_, Just _) = Left "Nested map values"
     in Map.fromList <$> traverse parseParam params

-- | Generate query parameters from an object, using the deep object syntax.
-- A result of @'(["a", "b", "c"], Just "d")'@ attributed to the @filter@
-- parameter name will result in the following query parameter:
-- @filter[a][b][c]=d@. Return decoded text: clients perform the required URL
-- encoding afterwards.
class ToDeepQuery a where
  toDeepQuery :: a -> [([Text], Maybe Text)]

-- | Turn a nested path into a deep object query param
--
-- >>> generateDeepParam "filter" (["a", "b", "c"], Just "d")
-- ("filter[a][b][c]",Just "d")
generateDeepParam :: Text -> ([Text], Maybe Text) -> (Text, Maybe Text)
generateDeepParam name (keys, value) =
  let makeKeySegment key = "[" <> key <> "]"
   in (name <> foldMap makeKeySegment keys, value)
