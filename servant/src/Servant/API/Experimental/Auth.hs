{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE PolyKinds          #-}
module Servant.API.Experimental.Auth where

import           Data.Typeable
                 (Typeable)

-- | A generalized Authentication combinator. Use this if you have a
-- non-standard authentication technique.
data AuthProtect (tag :: k) deriving (Typeable)
{-# DEPRECATED AuthProtect "Use https://github.com/haskell-servant/servant-auth instead" #-}
