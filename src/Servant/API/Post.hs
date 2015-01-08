{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures #-}
module Servant.API.Post where

import Data.Typeable ( Typeable )

-- | Endpoint for POST requests. The type variable represents the type of the
-- response body (not the request body, use 'Servant.API.ReqBody.ReqBody' for
-- that).
--
-- Example:
--
-- >            -- POST /books
-- >            -- with a JSON encoded Book as the request body
-- >            -- returning the just-created Book
-- > type MyApi = "books" :> ReqBody Book :> Post '[JSON] Book
data Post (cts::[*]) a
  deriving Typeable
