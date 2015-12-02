{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.API.Times where

import           Data.Typeable (Typeable)
import           GHC.TypeLits  (Symbol)


-- | Capture a value from the request path under a certain type @a@.
--
-- Example:
-- >>>            -- GET /books/:isbn
-- >>> type MyApi = "books" :> Capture "isbn" Text :> Get '[JSON] Book
data CaptureTime (sym :: Symbol) (format :: Symbol) a
    deriving (Typeable)


data QueryParamTime (sym :: Symbol) (format :: Symbol) a
    deriving (Typeable)


data QueryParamTimes (sym :: Symbol) (format :: Symbol) a
    deriving (Typeable)

