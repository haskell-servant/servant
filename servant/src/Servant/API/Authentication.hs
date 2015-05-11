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
-- >>> type AuthLookup = Text -> IO User
-- >>> type MyApi = BasicAuth "book-realm" :> "books" :> Get '[JSON] [Book]
data BasicAuth (realm :: Symbol) lookup
    deriving (Typeable)

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data User
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
