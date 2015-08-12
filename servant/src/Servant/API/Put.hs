{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures     #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Put (Put) where

import           Servant.API.Methods

-- | Endpoint for PUT requests, usually used to update a ressource.
-- The type @a@ is the type of the response body that's returned.
--
-- Example:
--
-- >>> -- PUT /books/:isbn
-- >>> -- with a Book as request body, returning the updated Book
-- >>> type MyApi = "books" :> Capture "isbn" Text :> ReqBody '[JSON] Book :> Put '[JSON] Book
type Put (contentTypes :: [*]) a = HttpMethod "PUT" 200 contentTypes a

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
