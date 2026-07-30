-- |
-- Module:      Servant.OpenApi.TypeLevel
-- License:     BSD3
-- Maintainer:  Nickolay Kudasov <nickolay@getshoptv.com>
-- Stability:   experimental
--
-- Useful type families for servant APIs.
module Servant.OpenApi.TypeLevel
  ( IsSubAPI
  , EndpointsList
  , BodyTypes
  ) where

import Servant.OpenApi.Internal.TypeLevel
