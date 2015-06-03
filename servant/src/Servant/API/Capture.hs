{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds          #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Capture (Capture) where

import           Data.Typeable (Typeable)
import           GHC.TypeLits  (Symbol)
-- | Capture a value from the request path under a certain type @a@.
--
-- Example:
--
-- >>>            -- GET /books/:isbn
-- >>> type MyApi = "books" :> Capture "isbn" Text :> Get '[JSON] Book
data Capture (sym :: Symbol) a
    deriving (Typeable)

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
