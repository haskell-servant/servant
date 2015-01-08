{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Servant.API.Get where

import Data.Typeable ( Typeable )

-- | Endpoint for simple GET requests. Serves the result as JSON.
--
-- Example:
--
-- > type MyApi = "books" :> Get '[JSON] [Book]
data Get (cts::[*]) a
  deriving Typeable
