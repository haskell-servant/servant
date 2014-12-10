{-# LANGUAGE PolyKinds #-}
module Servant.API.Capture (Capture) where

-- | Capture a value from the request path under a certain type @a@.
--
-- Example:
--
-- >            -- GET /books/:isbn
-- > type MyApi = "books" :> Capture "isbn" Text :> Get Book
data Capture sym a
