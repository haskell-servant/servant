{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Description (
    -- * Combinators
    Description,
    Summary,
    -- * Used as modifiers
    FoldDescription,
    FoldDescription',
    reflectDescription,
    ) where

import           Data.Proxy
                 (Proxy (..))
import           Data.Typeable
                 (Typeable)
import           GHC.TypeLits
                 (KnownSymbol, Symbol, symbolVal)

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
--  \Haskell multiline String support is not perfect \
--  \but it's still very readable."
-- :> Get '[JSON] Book
-- :}
data Description (sym :: Symbol)
    deriving (Typeable)

-- | Fold list of modifiers to extract description as a type-level String.
--
-- >>> :kind! FoldDescription '[]
-- FoldDescription '[] :: Symbol
-- = ""
--
-- >>> :kind! FoldDescription '[Required, Description "foobar", Lenient]
-- FoldDescription '[Required, Description "foobar", Lenient] :: Symbol
-- = "foobar"
--
type FoldDescription mods = FoldDescription' "" mods

-- | Implementation of 'FoldDescription'.
type family FoldDescription' (acc :: Symbol) (mods ::  [*]) :: Symbol where
    FoldDescription' acc '[]                        = acc
    FoldDescription' acc (Description desc ': mods) = FoldDescription' desc mods
    FoldDescription' acc (mod     ': mods)          = FoldDescription' acc mods

-- | Reflect description to the term level.
--
-- >>> reflectDescription (Proxy :: Proxy '[Required, Description "foobar", Lenient])
-- "foobar"
--
reflectDescription :: forall mods. KnownSymbol (FoldDescription mods) => Proxy mods -> String
reflectDescription _ = symbolVal (Proxy :: Proxy (FoldDescription mods))

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
-- >>> data SourceFile
-- >>> instance ToJSON SourceFile where { toJSON = undefined }
