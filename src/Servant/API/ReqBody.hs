{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Servant.API.ReqBody where

-- | Extract the request body as a value of type @a@.
--
-- Example:
--
-- >            -- POST /books
-- > type MyApi = "books" :> ReqBody '[JSON] Book :> Post Book
data ReqBody (ls::[*]) a
