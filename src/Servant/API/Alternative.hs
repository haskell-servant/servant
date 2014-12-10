{-# LANGUAGE TypeOperators #-}
module Servant.API.Alternative where

-- | Union of two APIs, first takes precedence in case of overlap.
--
-- Example:
--
-- > type MyApi = "books" :> Get [Book] -- GET /books
-- >         :<|> "books" :> ReqBody Book :> Post Book -- POST /books
data a :<|> b = a :<|> b
infixr 8 :<|>
