{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures     #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Get (Get) where

import           Data.Typeable (Typeable)

-- | Endpoint for simple GET requests. Serves the result as JSON.
--
-- Example:
--
-- >>> type MyApi = "books" :> Get '[JSON] [Book]
data Get (contentTypes :: [*]) a
  deriving Typeable

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
