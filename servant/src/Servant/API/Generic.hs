{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
-- | Define servant servers from record types. Generics for the win.
--
-- The usage is simple, if you only need a collection of routes.  First you
-- define a record with field types prefixed by a parameter `route`:
--
-- @
-- data Routes route = Routes
--     { _get :: route :- Capture "id" Int :> Get '[JSON] String
--     , _put :: route :- ReqBody '[JSON] Int :> Put '[JSON] Bool
--     }
--   deriving ('Generic')
-- @
--
-- You can get a 'Proxy' of the server using
--
-- @
-- api :: Proxy (ToServantApi Routes)
-- api = genericApi (Proxy :: Proxy Routes)
-- @
--
-- Using 'genericApi' is better as it checks that instances exists,
-- i.e. you get better error messages than simply using 'Proxy' value.
--
-- __Note:__ in 0.14 series this module isn't re-exported from 'Servant.API'.
--
-- "Servant.API.Generic" is based on @servant-generic@ package by
-- [Patrick Chilton](https://github.com/chpatrick)
--
-- @since 0.14.1
module Servant.API.Generic (
    GenericMode (..),
    GenericServant,
    ToServant,
    toServant,
    fromServant,
    -- * AsApi
    AsApi,
    ToServantApi,
    genericApi,
    -- * Utility
    GServantProduct,
    -- * re-exports
    Generic (Rep),
  ) where

-- Based on servant-generic licensed under MIT License
--
-- Copyright (c) 2017 Patrick Chilton
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

import           Data.Proxy
                 (Proxy (..))
import           GHC.Generics
                 ((:*:) (..), Generic (..), K1 (..), M1 (..))

import           Servant.API.Alternative

-- | A constraint alias, for work with 'mode' and 'routes'.
type GenericServant routes mode = (GenericMode mode, Generic (routes mode), GServantProduct (Rep (routes mode)))

-- | A class with a type family that applies an appropriate type family to the @api@
-- parameter.  For example, 'AsApi' will leave @api@ untouched, while
-- @'AsServerT' m@ will produce @'ServerT' api m@.
class GenericMode mode where
    type mode :- api :: *

infixl 0 :-

-- | Turns a generic product type into a tree of `:<|>` combinators.
type ToServant routes mode = GToServant (Rep (routes mode))

type ToServantApi routes = ToServant routes AsApi

-- | See `ToServant`, but at value-level.
toServant
    :: GenericServant routes mode
    => routes mode -> ToServant routes mode
toServant = gtoServant . from

-- | Inverse of `toServant`.
--
-- This can be used to turn 'generated' values such as client functions into records.
--
-- You may need to provide a type signature for the /output/ type (your record type).
fromServant
    :: GenericServant routes mode
    => ToServant routes mode -> routes mode
fromServant = to . gfromServant

-- | A type that specifies that an API record contains an API definition. Only useful at type-level.
data AsApi
instance GenericMode AsApi where
    type AsApi :- api = api

-- | Get a 'Proxy' of an API type.
genericApi
    :: GenericServant routes AsApi
    => Proxy routes
    -> Proxy (ToServantApi routes)
genericApi _ = Proxy

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------


class GServantProduct f where
    type GToServant f
    gtoServant   :: f p -> GToServant f
    gfromServant :: GToServant f -> f p

instance GServantProduct f => GServantProduct (M1 i c f) where
    type GToServant (M1 i c f) = GToServant f
    gtoServant   = gtoServant . unM1
    gfromServant = M1 . gfromServant

instance (GServantProduct l, GServantProduct r) => GServantProduct (l :*: r) where
    type GToServant (l :*: r) = GToServant l :<|> GToServant r
    gtoServant   (l :*: r)  = gtoServant l :<|> gtoServant r
    gfromServant (l :<|> r) = gfromServant l :*: gfromServant r

instance GServantProduct (K1 i c) where
    type GToServant (K1 i c) = c
    gtoServant   = unK1
    gfromServant = K1
