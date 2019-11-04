{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.OperationId (
    -- * Combinators
    OperationId,
    ) where

import           Data.Typeable
                 (Typeable)
import           GHC.TypeLits
                 (Symbol)

-- | Add an operation Id for (part of) API.
--
-- Example:
--
-- >>> type MyApi = OperationId "getBookById" :> "books" :> Capture "id" Int :> Get '[JSON] Book
data OperationId (sym :: Symbol)
    deriving (Typeable)

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
-- >>> data SourceFile
-- >>> instance ToJSON SourceFile where { toJSON = undefined }
