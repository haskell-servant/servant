{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module exports orphan instances to make
-- [@servant-queryparam-core@](https://hackage.haskell.org/package/servant-queryparam-core) work with [@servant-openapi3@](https://hackage.haskell.org/package/servant-openapi3).
module Servant.OpenApi.Record () where

import Data.OpenApi
import Data.Proxy
import Servant.API
import Servant.OpenApi
import Servant.Record

instance HasOpenApi (UnRecordParam mod (RecordParam mod a :> api)) => HasOpenApi (RecordParam mod a :> api) where
  toOpenApi :: Proxy (RecordParam mod a :> api) -> OpenApi
  toOpenApi _ = toOpenApi (Proxy :: Proxy (UnRecordParam mod (RecordParam mod a :> api)))
