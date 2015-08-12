{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures     #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Delete (Delete) where

import           Servant.API.Methods

-- | Combinator for DELETE requests.
--
-- Example:
--
-- >>>            -- DELETE /books/:isbn
-- >>> type MyApi = "books" :> Capture "isbn" Text :> Delete '[JSON] ()
type Delete (contentTypes :: [*]) a = HttpMethod "DELETE" 200 contentTypes a

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
