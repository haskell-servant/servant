{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Servant.Server.Internal.Context where

import           Data.Proxy
import           GHC.TypeLits

-- | 'Context's are used to pass values to combinators. (They are __not__ meant
-- to be used to pass parameters to your handlers, i.e. they should not replace
-- any custom 'Control.Monad.Trans.Reader.ReaderT'-monad-stack that you're using
-- with 'hoistServer'.) If you don't use combinators that
-- require any context entries, you can just use 'Servant.Server.serve' as always.
--
-- If you are using combinators that require a non-empty 'Context' you have to
-- use 'Servant.Server.serveWithContext' and pass it a 'Context' that contains all
-- the values your combinators need. A 'Context' is essentially a heterogeneous
-- list and accessing the elements is being done by type (see 'getContextEntry').
-- The parameter of the type 'Context' is a type-level list reflecting the types
-- of the contained context entries. To create a 'Context' with entries, use the
-- operator @(':.')@:
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

-- | Append two type-level lists.
--
-- Hint: import it as
--
-- > import Servant.Server (type (.++))
type family (.++) (l1 :: [*]) (l2 :: [*]) where
  '[] .++ a = a
  (a ': as) .++ b = a ': (as .++ b)

-- | Append two contexts.
(.++) :: Context l1 -> Context l2 -> Context (l1 .++ l2)
EmptyContext .++ a = a
(a :. as) .++ b = a :. (as .++ b)

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
-- ...No instance for (HasContextEntry '[] [Char])
-- ...
class HasContextEntry (context :: [*]) (val :: *) where
    getContextEntry :: Context context -> val

instance {-# OVERLAPPABLE #-}
         HasContextEntry xs val => HasContextEntry (notIt ': xs) val where
    getContextEntry (_ :. xs) = getContextEntry xs

instance {-# OVERLAPPING #-}
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
