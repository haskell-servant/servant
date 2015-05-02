{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds          #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.ReqBody where

import           Data.Typeable (Typeable)
-- | Extract the request body as a value of type @a@.
--
-- Example:
--
-- >>>            -- POST /books
-- >>> type MyApi = "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book
data ReqBody (contentTypes :: [*]) a
    deriving (Typeable)

-- $setup
-- >>> import Servant.API
-- >>> import Servant.Common.Text
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
