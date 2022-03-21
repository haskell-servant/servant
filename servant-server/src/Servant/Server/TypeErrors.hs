{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans         #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

#if __GLASGOW_HASKELL__ >= 904
{-# LANGUAGE TypeApplications       #-}
#endif

-- | This module contains erroring instances for @Servant.Server.Internal@.
-- They are separated from the bulk of the code, because they raise "missing methods"
-- warnings. These warnings are expected, but ignoring them would lead to missing
-- relevant warnings in @SServant.Server.Internal@. Therefore, we put them in a separate
-- file, and ignore the warnings here.
module Servant.Server.TypeErrors ()
  where

import           Data.Constraint (Constraint)
import           GHC.TypeLits
                 (TypeError)
import           Prelude ()
import           Prelude.Compat
import           Servant.API
                 ((:>))
import           Servant.API.TypeErrors

import           Servant.Server.Internal

import           GHC.TypeLits
                 (ErrorMessage (..))

#if __GLASGOW_HASKELL__ >= 904
import           Data.Kind (Type)
#endif

-- Erroring instance for 'HasServer' when a combinator is not fully applied
instance TypeError (PartialApplication 
#if __GLASGOW_HASKELL__ >= 904
                    @(Type -> [Type] -> Constraint) 
#endif
                    HasServer arr) => HasServer ((arr :: a -> b) :> sub) context
  where
    type ServerT (arr :> sub) _ = TypeError (PartialApplication (HasServer :: * -> [*] -> Constraint) arr)
    route = error "unreachable"
    hoistServerWithContext _ _ _ _ = error "unreachable"

-- | This instance prevents from accidentally using '->' instead of ':>'
--
-- >>> serve (Proxy :: Proxy (Capture "foo" Int -> Get '[JSON] Int)) (error "...")
-- ...
-- ...No instance HasServer (a -> b).
-- ...Maybe you have used '->' instead of ':>' between
-- ...Capture' '[] "foo" Int
-- ...and
-- ...Verb 'GET 200 '[JSON] Int
-- ...
--
-- >>> undefined :: Server (Capture "foo" Int -> Get '[JSON] Int)
-- ...
-- ...No instance HasServer (a -> b).
-- ...Maybe you have used '->' instead of ':>' between
-- ...Capture' '[] "foo" Int
-- ...and
-- ...Verb 'GET 200 '[JSON] Int
-- ...
--
instance TypeError (HasServerArrowTypeError a b) => HasServer (a -> b) context
  where
    type ServerT (a -> b) m = TypeError (HasServerArrowTypeError a b)
    route _ _ _ = error "servant-server panic: impossible happened in HasServer (a -> b)"
    hoistServerWithContext _ _ _ = id

type HasServerArrowTypeError a b =
    'Text "No instance HasServer (a -> b)."
    ':$$: 'Text "Maybe you have used '->' instead of ':>' between "
    ':$$: 'ShowType a
    ':$$: 'Text "and"
    ':$$: 'ShowType b

-- Erroring instances for 'HasServer' for unknown API combinators

-- XXX: This omits the @context@ parameter, e.g.:
--
-- "There is no instance for HasServer (Bool :> …)". Do we care ?
instance {-# OVERLAPPABLE #-} TypeError (NoInstanceForSub 
#if __GLASGOW_HASKELL__ >= 904
                                         @(Type -> [Type] -> Constraint) 
#endif
                                         HasServer ty) => HasServer (ty :> sub) context

instance {-# OVERLAPPABLE #-} TypeError (NoInstanceFor (HasServer api context)) => HasServer api context

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XTypeOperators
-- >>> import Data.Typeable
-- >>> import Servant.API
-- >>> import Servant.Server
