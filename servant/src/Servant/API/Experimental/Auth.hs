{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE PolyKinds          #-}
module Servant.API.Experimental.Auth where

import           Data.Typeable
                 (Typeable)

-- | A generalized Authentication combinator. Use this if you have a
-- non-standard authentication technique.
--
-- NOTE: THIS API IS EXPERIMENTAL AND SUBJECT TO CHANGE.
data AuthProtect (tag :: k) deriving (Typeable)
