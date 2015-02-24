{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds          #-}
module Servant.API.Header (Header) where

import           Data.Typeable (Typeable)
import           GHC.TypeLits  (Symbol)
-- | Extract the given header's value as a value of type @a@.
--
-- Example:
--
-- >>> newtype Referer = Referer Text deriving (Eq, Show)
-- >>>
-- >>>            -- GET /view-my-referer
-- >>> type MyApi = "view-my-referer" :> Header "from" Referer :> Get '[JSON] Referer
data Header (sym :: Symbol) a
    deriving Typeable

-- $setup
-- >>> import Servant.API
-- >>> import Servant.Common.Text
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
