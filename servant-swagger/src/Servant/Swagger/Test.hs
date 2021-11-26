-- |
-- Module:      Servant.Swagger.Test
-- License:     BSD3
-- Maintainer:  Nickolay Kudasov <nickolay@getshoptv.com>
-- Stability:   experimental
--
-- Automatic tests for servant API against Swagger spec.
module Servant.Swagger.Test (
  validateEveryToJSON,
  validateEveryToJSONWithPatternChecker,
) where

import           Servant.Swagger.Internal.Test
