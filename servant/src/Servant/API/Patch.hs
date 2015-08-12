{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures     #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Patch (Patch) where

import           Servant.API.Methods

-- | Endpoint for PATCH requests. The type variable represents the type of the
-- response body (not the request body, use 'Servant.API.ReqBody.ReqBody' for
-- that).
--
-- If the HTTP response is empty, only () is supported.
--
-- Example:
--
-- >>>            -- PATCH /books
-- >>>            -- with a JSON encoded Book as the request body
-- >>>            -- returning the just-created Book
-- >>> type MyApi = "books" :> ReqBody '[JSON] Book :> Patch '[JSON] Book
type Patch (contentTypes :: [*]) a = HttpMethod "PATCH" 200 contentTypes a

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
