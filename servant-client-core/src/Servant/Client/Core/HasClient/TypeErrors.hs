{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans         #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- | This module contains erroring instances for @Servant.Client.Core.HasClient.Internal@.
-- They are separated from the bulk of the code, because they raise "missing methods"
-- warnings. These warnings are expected, but ignoring them would lead to missing
-- relevant warnings in @Servant.Client.Core.HasClient.Internal@. Therefore, we put them
-- in a separate file, and ignore the warnings here.
module Servant.Client.Core.HasClient.TypeErrors ()
  where

import           Prelude ()
import           Prelude.Compat

import           GHC.TypeLits
                 (TypeError)
import           Servant.API
                 ((:>))
import           Servant.API.TypeErrors

import           Servant.Client.Core.HasClient.Internal
import           Servant.Client.Core.RunClient

-- Erroring instance for HasClient' when a combinator is not fully applied
instance (RunClient m, TypeError (PartialApplication HasClient arr)) => HasClient m ((arr :: a -> b) :> sub)
  where
    type Client m (arr :> sub) = TypeError (PartialApplication HasClient arr)
    clientWithRoute _ _ _ = error "unreachable"
    hoistClientMonad _ _ _ _ = error "unreachable"

-- Erroring instances for 'HasClient' for unknown API combinators
instance {-# OVERLAPPABLE #-} (RunClient m, TypeError (NoInstanceForSub (HasClient m) ty)) => HasClient m (ty :> sub)

instance {-# OVERLAPPABLE #-} (RunClient m, TypeError (NoInstanceFor (HasClient m api))) => HasClient m api
