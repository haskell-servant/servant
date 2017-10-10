{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds          #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Description (Description, Summary) where

import           Data.Typeable (Typeable)
import           GHC.TypeLits  (Symbol)
-- | Add a short summary for (part of) API.
--
-- Example:
--
-- >>> type MyApi = Summary "Get book by ISBN." :> "books" :> Capture "isbn" Text :> Get '[JSON] Book
data Summary (sym :: Symbol)
    deriving (Typeable)

-- | Add more verbose description for (part of) API.
--
-- Example:
--
-- >>> :{
--type MyApi = Description
--  "This comment is visible in multiple Servant interpretations \
--  \and can be really long if necessary. \
--  \Haskell multiline support is not perfect \
--  \but it's still very readable."
-- :> Get '[JSON] Book
-- :}
data Description (sym :: Symbol)
    deriving (Typeable)

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
-- >>> data SourceFile
-- >>> instance ToJSON SourceFile where { toJSON = undefined }
