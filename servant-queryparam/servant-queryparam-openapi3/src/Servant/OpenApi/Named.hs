{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module exports orphan instances to make
-- [@servant-queryparam-core@](https://hackage.haskell.org/package/servant-queryparam-core) work with servers.
module Servant.OpenApi.Named () where

import Data.OpenApi
import Data.Proxy
import Servant.API
import Servant.Named
import Servant.OpenApi

instance HasOpenApi (UnNameParam (NamedQueryParams sym a :> api)) => HasOpenApi (NamedQueryParams sym a :> api) where
  toOpenApi :: Proxy (NamedQueryParams sym a :> api) -> OpenApi
  toOpenApi _ = toOpenApi (Proxy :: Proxy (UnNameParam (NamedQueryParams sym a :> api)))

instance HasOpenApi (UnNameParam (NamedQueryParam' mods sym a :> api)) => HasOpenApi (NamedQueryParam' mods sym a :> api) where
  toOpenApi :: Proxy (NamedQueryParam' mods sym a :> api) -> OpenApi
  toOpenApi _ = toOpenApi (Proxy :: Proxy (UnNameParam (NamedQueryParam' mods sym a :> api)))

instance HasOpenApi (UnNameParam (NamedQueryFlag sym :> api)) => HasOpenApi (NamedQueryFlag sym :> api) where
  toOpenApi :: Proxy (NamedQueryFlag sym :> api) -> OpenApi
  toOpenApi _ = toOpenApi (Proxy :: Proxy (UnNameParam (NamedQueryFlag sym :> api)))
