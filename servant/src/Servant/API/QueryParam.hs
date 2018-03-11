{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.QueryParam (QueryFlag, QueryParam, QueryParam', QueryParams) where

import           Data.Typeable
                 (Typeable)
import           GHC.TypeLits
                 (Symbol)
import           Servant.API.Modifiers

-- | Lookup the value associated to the @sym@ query string parameter
-- and try to extract it as a value of type @a@.
--
-- Example:
--
-- >>> -- /books?author=<author name>
-- >>> type MyApi = "books" :> QueryParam "author" Text :> Get '[JSON] [Book]
type QueryParam = QueryParam' '[Optional, Strict]

-- | 'QueryParam' which can be 'Required', 'Lenient', or modified otherwise.
data QueryParam' (mods :: [*]) (sym :: Symbol) (a :: *)
    deriving Typeable

-- | Lookup the values associated to the @sym@ query string parameter
-- and try to extract it as a value of type @[a]@. This is typically
-- meant to support query string parameters of the form
-- @param[]=val1&param[]=val2@ and so on. Note that servant doesn't actually
-- require the @[]@s and will fetch the values just fine with
-- @param=val1&param=val2@, too.
--
-- Example:
--
-- >>> -- /books?authors[]=<author1>&authors[]=<author2>&...
-- >>> type MyApi = "books" :> QueryParams "authors" Text :> Get '[JSON] [Book]
data QueryParams (sym :: Symbol) (a :: *)
    deriving Typeable

-- | Lookup a potentially value-less query string parameter
-- with boolean semantics. If the param @sym@ is there without any value,
-- or if it's there with value "true" or "1", it's interpreted as 'True'.
-- Otherwise, it's interpreted as 'False'.
--
-- Example:
--
-- >>> -- /books?published
-- >>> type MyApi = "books" :> QueryFlag "published" :> Get '[JSON] [Book]
data QueryFlag (sym :: Symbol)

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
