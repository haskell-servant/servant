{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Get (Get) where

import           Servant.API.Methods

-- | Endpoint for simple GET requests. Serves the result as JSON.
--
-- Example:
--
-- >>> type MyApi = "books" :> Get '[JSON] [Book]
type Get (contentTypes :: [*]) a = HttpMethod "GET" contentTypes a

type instance DefaultStatusCode "GET" = 200

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
