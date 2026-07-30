-- |
-- Module:      Servant.OpenApi.Test
-- License:     BSD3
-- Maintainer:  Nickolay Kudasov <nickolay@getshoptv.com>
-- Stability:   experimental
--
-- Automatic tests for servant API against OpenApi spec.
module Servant.OpenApi.Test
  ( validateEveryToJSON
  , validateEveryToJSONWithPatternChecker
  ) where

import Servant.OpenApi.Internal.Test
