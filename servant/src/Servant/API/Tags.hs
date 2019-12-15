{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Tags (
    SymbolVals(..),
    -- * Combinators
    Tags,
    ) where

import           Data.Proxy
                 (Proxy (..))
import           Data.Typeable
                 (Typeable)
import           GHC.TypeLits
                 (KnownSymbol, Symbol, symbolVal)

-- | Add tags for (part of) API.
--
-- Example:
--
-- >>> type MyApi = Tags '["Books"] :> "books" :> Capture "id" Int :> Get '[JSON] Book
class SymbolVals a where
    symbolVals :: proxy a -> [String]

instance SymbolVals '[] where
    symbolVals _ = []

instance (KnownSymbol h, SymbolVals t) => SymbolVals (h ': t) where
    symbolVals _ = symbolVal (Proxy :: Proxy h) : symbolVals (Proxy :: Proxy t)

data Tags (tags :: [Symbol])
    deriving (Typeable)

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
-- >>> data SourceFile
-- >>> instance ToJSON SourceFile where { toJSON = undefined }