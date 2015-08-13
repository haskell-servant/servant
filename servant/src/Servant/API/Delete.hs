{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Delete (Delete) where

import           Servant.API.Methods

-- | Combinator for DELETE requests.
--
-- Example:
--
-- >>>            -- DELETE /books/:isbn
-- >>> type MyApi = "books" :> Capture "isbn" Text :> Delete '[JSON] ()
type Delete (contentTypes :: [*]) a = HttpMethod "DELETE" contentTypes a

type instance DefaultStatusCode "DELETE" = 200

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
