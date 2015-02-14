{-# LANGUAGE DeriveDataTypeable #-}
module Servant.API.Patch where

import Data.Typeable ( Typeable )

-- | Endpoint for PATCH requests. The type variable represents the type of the
-- response body (not the request body, use 'Servant.API.ReqBody.ReqBody' for
-- that).
--
-- If the HTTP response is empty, only () is supported.
--
-- Example:
--
-- >            -- POST /books
-- >            -- with a JSON encoded Book as the request body
-- >            -- returning the just-created Book
-- > type MyApi = "books" :> ReqBody Book :> Post Book
data Patch a
  deriving Typeable
