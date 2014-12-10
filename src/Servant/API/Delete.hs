{-# LANGUAGE DeriveDataTypeable #-}
module Servant.API.Delete where

import Data.Typeable

-- | Combinator for DELETE requests.
--
-- Example:
--
-- >            -- DELETE /books/:isbn
-- > type MyApi = "books" :> Capture "isbn" Text :> Delete
data Delete
  deriving Typeable
