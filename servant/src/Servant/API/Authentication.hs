{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds          #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Authentication (BasicAuth) where

import           Data.Typeable (Typeable)
import           GHC.TypeLits  (Symbol)

-- | Basic Authentication with respect to a specified @realm@ and a @lookup@
-- type to encapsulate authentication logic.
--
-- Example:
-- >>> type MyApi = BasicAuth "book-realm" DB :> "books" :> Get '[JSON] [Book]
data BasicAuth (realm :: Symbol) lookup a
    deriving (Typeable)

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data DB
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
