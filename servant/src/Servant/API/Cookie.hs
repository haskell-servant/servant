{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE PolyKinds          #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Cookie where

import Data.ByteString    (ByteString)
import Data.Typeable      (Typeable)
import GHC.TypeLits       (Symbol)

-- | Extract the given cookie's value as a value of type @a@.
--
-- Example:
--
-- >>>            -- GET /test
-- >>> type MyApi = "test" :> Cookie "token" Text :> Get '[JSON] Text

data Cookie (sym :: Symbol) a = Cookie a
                              | MissingCookie
                              | UndecodableCookie ByteString
                              deriving (Typeable, Eq, Show, Functor)

