module Servant.API.ReqBody where

-- | Extract the request body as a value of type @a@.
--
-- Example:
--
-- >            -- POST /books
-- > type MyApi = "books" :> ReqBody Book :> Post Book
data ReqBody a
