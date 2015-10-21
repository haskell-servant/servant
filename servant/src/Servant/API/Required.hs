{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Required (
  Required,
  RequiredParamType,
) where

import Data.Typeable (Typeable)

-- | Make an othwise optional parameter required.
--
-- Example:
--
-- >>> -- /books?author=<author name>
-- >>> type MyApi = "books" :> Required (QueryParam "author" Text) :> Get '[JSON] [Book]
data Required a
  deriving Typeable

-- | Get the @a@ from @Maybe a -> b@.
type family RequiredParamType a :: * where
  RequiredParamType (Maybe a -> b) = a

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
