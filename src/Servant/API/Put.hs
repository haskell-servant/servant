{-# LANGUAGE DeriveDataTypeable #-}
module Servant.API.Put where

import Data.Typeable ( Typeable )

-- | Endpoint for PUT requests, usually used to update a ressource.
-- The type @a@ is the type of the response body that's returned.
--
-- Example:
--
-- > -- PUT /books/:isbn
-- > -- with a Book as request body, returning the updated Book
-- > type MyApi = "books" :> Capture "isbn" Text :> ReqBody Book :> Put Book
data Put a
  deriving Typeable
