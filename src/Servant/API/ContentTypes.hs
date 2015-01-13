{-# LANGUAGE DeriveDataTypeable #-}
module Servant.API.ContentTypes where

import Data.Typeable


data XML deriving Typeable
data HTML deriving Typeable
data JSON deriving Typeable
data JavaScript deriving Typeable
data CSS deriving Typeable
data PlainText deriving Typeable
data OctetStream deriving Typeable
