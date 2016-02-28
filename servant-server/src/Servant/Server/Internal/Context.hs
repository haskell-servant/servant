{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

#include "overlapping-compat.h"

module Servant.Server.Internal.Context where

import           Data.Proxy
import           GHC.TypeLits

-- | When calling 'Servant.Server.serve' you have to supply a context
-- value of type @'Context' contextTypes@. This parameter is used to pass values
-- to combinators. (It shouldn't be confused with general configuration
-- parameters for your web app, like the port, etc.). If you don't use
-- combinators that require any context entries, you can just use `serve` (or
-- pass 'EmptyContext'). To create a context with entries, use the operator
-- @(':.')@. The parameter of the type 'Context' is a type-level list reflecting
-- the types of the contained context entries:
--
-- >>> :type True :. () :. EmptyContext
-- True :. () :. EmptyContext :: Context '[Bool, ()]
data Context contextTypes where
    EmptyContext :: Context '[]
    (:.) :: x -> Context xs -> Context (x ': xs)
infixr 5 :.

instance Show (Context '[]) where
  show EmptyContext = "EmptyContext"
instance (Show a, Show (Context as)) => Show (Context (a ': as)) where
  showsPrec outerPrecedence (a :. as) =
    showParen (outerPrecedence > 5) $
      shows a . showString " :. " . shows as

instance Eq (Context '[]) where
    _ == _ = True
instance (Eq a, Eq (Context as)) => Eq (Context (a ': as)) where
    x1 :. y1 == x2 :. y2 = x1 == x2 && y1 == y2

-- | This class is used to access context entries in 'Context's. 'getContextEntry'
-- returns the first value where the type matches:
--
-- >>> getContextEntry (True :. False :. EmptyContext) :: Bool
-- True
--
-- If the 'Context' does not contain an entry of the requested type, you'll get
-- an error:
--
-- >>> getContextEntry (True :. False :. EmptyContext) :: String
-- ...
--     No instance for (HasContextEntry '[] [Char])
-- ...
class HasContextEntry (context :: [*]) (val :: *) where
    getContextEntry :: Context context -> val

instance OVERLAPPABLE_
         HasContextEntry xs val => HasContextEntry (notIt ': xs) val where
    getContextEntry (_ :. xs) = getContextEntry xs

instance OVERLAPPING_
         HasContextEntry (val ': xs) val where
    getContextEntry (x :. _) = x

-- * support for named subcontexts

-- | Normally context entries are accessed by their types. In case you need
-- to have multiple values of the same type in your 'Context' and need to access
-- them, we provide 'NamedContext'. You can think of it as sub-namespaces for
-- 'Context's.
data NamedContext (name :: Symbol) (subContext :: [*])
  = NamedContext (Context subContext)

-- | 'descendIntoNamedContext' allows you to access `NamedContext's. Usually you
-- won't have to use it yourself but instead use a combinator like
-- 'Servant.API.WithNamedContext.WithNamedContext'.
--
-- This is how 'descendIntoNamedContext' works:
--
-- >>> :set -XFlexibleContexts
-- >>> let subContext = True :. EmptyContext
-- >>> :type subContext
-- subContext :: Context '[Bool]
-- >>> let parentContext = False :. (NamedContext subContext :: NamedContext "subContext" '[Bool]) :. EmptyContext
-- >>> :type parentContext
-- parentContext :: Context '[Bool, NamedContext "subContext" '[Bool]]
-- >>> descendIntoNamedContext (Proxy :: Proxy "subContext") parentContext :: Context '[Bool]
-- True :. EmptyContext
descendIntoNamedContext :: forall context name subContext .
  HasContextEntry context (NamedContext name subContext) =>
  Proxy (name :: Symbol) -> Context context -> Context subContext
descendIntoNamedContext Proxy context =
  let NamedContext subContext = getContextEntry context :: NamedContext name subContext
  in subContext
