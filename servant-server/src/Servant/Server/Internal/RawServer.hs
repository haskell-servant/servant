{-|
Module      : Servant.Server.Internal.RawServer
Description : Definition of the RawServer type.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE KindSignatures #-}

module Servant.Server.Internal.RawServer (

      RawServer(..)

    ) where

import Network.Wai (Application)

-- | A Wai Application, but with a phantom type.
newtype RawServer (m :: * -> *) = RawServer {
      getRawServer :: Application
    }
