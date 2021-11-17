-- |
-- Module:      Servant.Swagger.TypeLevel
-- License:     BSD3
-- Maintainer:  Nickolay Kudasov <nickolay@getshoptv.com>
-- Stability:   experimental
--
-- Useful type families for servant APIs.
module Servant.Swagger.TypeLevel (
  IsSubAPI,
  EndpointsList,
  BodyTypes,
) where

import           Servant.Swagger.Internal.TypeLevel

