{-# LANGUAGE CPP                    #-}

module Servant.Client.TestServer (
  buildTestServer,
  withTestServer,
)where

#ifdef __GHCJS__
import           Servant.Client.TestServer.GHCJS
#else
import           Servant.Client.TestServer.GHC
#endif
