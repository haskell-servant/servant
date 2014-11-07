{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Servant.Client where

import Data.Proxy

import Servant.Utils.Req

-- * Accessing APIs as a Client

-- | 'client' allows you to produce operations to query an API from a client.
client :: HasClient layout => Proxy layout -> Client layout
client p = clientWithRoute p defReq

class HasClient layout where
  type Client layout :: *
  clientWithRoute :: Proxy layout -> Req -> Client layout
