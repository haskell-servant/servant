{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.QueryString (QueryString, DeepQuery) where

import           Data.Typeable
                 (Typeable)
import           GHC.TypeLits
                 (Symbol)

-- | Extract the whole query string from a request. This is useful for query strings
-- containing dynamic parameter names. For query strings with static parameter names,
-- 'QueryParam' is more suited.
--
-- Example:
--
-- >>> -- /books?author=<author name>&year=<book year>
-- >>> type MyApi = "books" :> QueryString :> Get '[JSON] [Book]
data QueryString
    deriving Typeable

-- | Extract an deep object from a query string.
--
-- Example:
--
-- >>> -- /books?filter[author][name]=<author name>&filter[year]=<book year>
-- >>> type MyApi = "books" :> DeepQuery "filter" BookQuery :> Get '[JSON] [Book]
data DeepQuery (sym :: Symbol) (a :: *)
    deriving Typeable

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> data BookQuery
-- >>> instance ToJSON Book where { toJSON = undefined }
