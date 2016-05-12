{-# LANGUAGE CPP                    #-}

module Servant.Client.TestServer (
  buildTestServer,
  TestServer(..),
  withServer,
) where

#ifdef __GHCJS__
import           Servant.Client.TestServer.GHCJS
#else
import           Servant.Client.TestServer.GHC
#endif

import           Servant.Client.TestServer.Types
