{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds          #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Capture (Capture, Capture', CaptureAll, CaptureMany) where

import           Data.Typeable
                 (Typeable)
import           GHC.TypeLits
                 (Symbol)
-- | Capture a value from the request path under a certain type @a@.
--
-- Example:
--
-- >>>            -- GET /books/:isbn
-- >>> type MyApi = "books" :> Capture "isbn" Text :> Get '[JSON] Book
type Capture = Capture' '[] -- todo

-- | 'Capture' which can be modified. For example with 'Description'.
data Capture' (mods :: [*]) (sym :: Symbol) (a :: *)
    deriving (Typeable)

-- | Capture all remaining values from the request path under a certain type
-- @a@.
--
-- Example:
--
-- >>>            -- GET /src/*
-- >>> type MyAPI = "src" :> CaptureAll "segments" Text :> Get '[JSON] SourceFile
data CaptureAll (sym :: Symbol) (a :: *)
    deriving (Typeable)

-- | Non-greedily capture many remaining values from the request path under a
-- certain type @a@.
--
-- Example:
--
-- Where @<name>@ comprises one or more path segments:
--
-- >>>            -- GET /v2/<name>/tags/list
-- >>> type MyAPI = "v2" :> CaptureMany "name" Text :> "tags" :> "list" :> Get '[JSON] [Text]
--
-- Which would capture a @[Text]@ for @"name"@.
data CaptureMany (sym :: Symbol) (a :: *)
    deriving (Typeable)

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
-- >>> data SourceFile
-- >>> instance ToJSON SourceFile where { toJSON = undefined }
