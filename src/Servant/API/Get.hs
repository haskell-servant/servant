{-# LANGUAGE DeriveDataTypeable #-}
module Servant.API.Get where

import Data.Typeable ( Typeable )

-- | Endpoint for simple GET requests. Serves the result as JSON.
--
-- Example:
--
-- > type MyApi = "books" :> Get [Book]
data Get a
  deriving Typeable
