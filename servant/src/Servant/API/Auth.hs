{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE PolyKinds          #-}
module Servant.API.Auth where

import           Data.Typeable (Typeable)

-- | A generalized Authentication combinator. Use this if you have a
-- non-standard authentication technique.
data AuthProtect (tag :: k) deriving (Typeable)

