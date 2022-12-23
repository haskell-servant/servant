{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-# OPTIONS_GHC -fno-warn-orphans         #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

#if __GLASGOW_HASKELL__ >= 904
{-# LANGUAGE TypeApplications       #-}
#endif

-- | This module contains erroring instances for @Servant.Links.Internal@.
-- They are separated from the bulk of the code, because they raise "missing methods"
-- warnings. These warnings are expected, but ignoring them would lead to missing
-- relevant warnings in @Servant.Links.Internal@. Therefore, we put them in a separate
-- file, and ignore the warnings here.
module Servant.Links.TypeErrors ()
  where

import           Data.Constraint
import           GHC.TypeLits
                 (TypeError)
import           Prelude ()
import           Prelude.Compat

import           Servant.API.Sub
                 (type (:>))
import           Servant.API.TypeErrors
import           Servant.Links.Internal

#if __GLASGOW_HASKELL__ >= 904
import           Data.Kind (Type)
#endif

-- Erroring instance for 'HasLink' when a combinator is not fully applied
instance TypeError (PartialApplication 
#if __GLASGOW_HASKELL__ >= 904
                    @(Type -> Constraint) 
#endif
                    HasLink arr) => HasLink ((arr :: a -> b) :> sub)
  where
    type MkLink (arr :> sub) _ = TypeError (PartialApplication (HasLink :: * -> Constraint) arr)
    toLink = error "unreachable"

-- Erroring instances for 'HasLink' for unknown API combinators
instance {-# OVERLAPPABLE #-} TypeError (NoInstanceForSub 
#if __GLASGOW_HASKELL__ >= 904
                                         @(Type -> Constraint) 
#endif
                                         HasLink ty) => HasLink (ty :> sub)

instance {-# OVERLAPPABLE #-} TypeError (NoInstanceFor (HasLink api)) => HasLink api
