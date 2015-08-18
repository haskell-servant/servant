{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures     #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Delete (Delete) where

import           Data.Typeable (Typeable)

-- | Combinator for DELETE requests.
--
-- Example:
--
-- >>>            -- DELETE /books/:isbn
-- >>> type MyApi = "books" :> Capture "isbn" Text :> Delete
data Delete (contentTypes :: [*]) a
  deriving Typeable


-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
