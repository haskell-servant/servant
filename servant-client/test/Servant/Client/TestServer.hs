{-# LANGUAGE CPP                    #-}

-- | Testing works very differently under ghc and ghcjs. This module acts as a
-- CPP switch and import different modules depending on the used compiler (ghc
-- or ghcjs). Both imported modules provide the same API.
module Servant.Client.TestServer (
  buildTestServer,
  TestServer(..),
  withServer,
)where

#ifdef __GHCJS__
import           Servant.Client.TestServer.GHCJS
#else
import           Servant.Client.TestServer.GHC
#endif

import           Servant.Client.TestServer.Types
