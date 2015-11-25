{-# LANGUAGE CPP #-}

module Servant.Client.PerformRequest (
  ServantError(..),
  performHttpRequest,
) where

import           Servant.Client.PerformRequest.Base
#ifdef __GHCJS__
import           Servant.Client.PerformRequest.GHCJS
#else
import           Servant.Client.PerformRequest.GHC
#endif
