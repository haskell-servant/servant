{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Raw where

import           Data.Typeable (Typeable)
-- | Endpoint for plugging in your own Wai 'Application's.
--
-- The given 'Application' will get the request as received by the server, potentially with
-- a modified (stripped) 'pathInfo' if the 'Application' is being routed with 'Servant.API.Sub.:>'.
--
-- In addition to just letting you plug in your existing WAI 'Application's,
-- this can also be used with 'Servant.Utils.StaticFiles.serveDirectory' to serve
-- static files stored in a particular directory on your filesystem
data Raw deriving Typeable
