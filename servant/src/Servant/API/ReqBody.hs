{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds          #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.ReqBody (
    ReqBody, ReqBody',
    ) where

import           Data.Typeable
                 (Typeable)
import           Servant.API.Modifiers

-- | Extract the request body as a value of type @a@.
--
-- Example:
--
-- >>>            -- POST /books
-- >>> type MyApi = "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book
type ReqBody = ReqBody' '[Required, Strict]

-- |
--
-- /Note:/ 'ReqBody'' is always 'Required'.
data ReqBody' (mods :: [*]) (contentTypes :: [*]) (a :: *)
    deriving (Typeable)

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
