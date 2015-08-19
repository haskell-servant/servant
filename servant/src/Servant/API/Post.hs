{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures     #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Post (Post) where

import           Servant.API.Methods

-- | Endpoint for POST requests. The type variable represents the type of the
-- response body (not the request body, use 'Servant.API.ReqBody.ReqBody' for
-- that).
--
-- Example:
--
-- >>>            -- POST /books
-- >>>            -- with a JSON encoded Book as the request body
-- >>>            -- returning the just-created Book
-- >>> type MyApi = "books" :> ReqBody '[JSON] Book :> Post '[JSON] Book
type Post (contentTypes :: [*]) a = HttpMethod "POST" 201 contentTypes a

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
