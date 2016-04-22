-- | This module contains all logic related to constructing or using
-- @Predicates@.
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.QuickCheck.Internal.Predicates where

import           Data.Aeson         (ToJSON (toJSON), Value (..))
import           Data.Proxy         (Proxy (..))
import           Data.Void
import           Network.HTTP.Types (statusCode)
import           Servant.Common.Req (ServantError (..))
import           Test.QuickCheck


-- | An HList containing predicates (functions of type @a -> Bool@). This
-- datatype is used to represent both filters (what values to discard when
-- generating arguments to test an API) and tests results (what to consider a
-- failing response).
--
-- For both filters and test results, only the *first* predicate of the
-- appropriate type is used.
--
-- Use 'emptyPredicates', 'addPredicate', 'addLeftPredicate' and
-- 'addRightPredicate' to construct a @Predicates@.
data Predicates a where
    HNil :: Predicates '[]
    HCons :: (a -> Bool) -> Predicates b -> Predicates (a ': b)
    HConsC :: Constraint a -> Predicates b -> Predicates (Constraint a ': b)

class HasPredicate a b where
    getPredicate :: Predicates a -> b -> Bool

instance {-# OVERLAPPING #-} HasPredicate '[] a where
    getPredicate _ = const True

-- TODO: Find some better way of distinguishing how the predicate is being used
instance {-# OVERLAPPING #-} HasPredicate '[] (Either ServantError a) where
    getPredicate _ = discard

instance {-# OVERLAPPING #-} HasPredicate (a ': xs) a where
    getPredicate (HCons a _) = a
    getPredicate (HConsC _ _) = error "not impossible, but non-sensical"

data Constraint ctx = Constraint
  { getConstraint :: forall a . (ctx a) => a -> Bool }

-- This is a little bit of a hack. Ideally instances would match when the
-- predicate is polymorphic, but that doesn't work since the polymorphic type
-- may have to unify with multiple distict values.
--
-- It may however be possible to define a MPTC from monomorphic to polymorphic
-- datatypes to avoid this issue.
instance {-# OVERLAPPING #-}
    HasPredicate (Either ServantError Void ': xs) (Either ServantError a) where
    getPredicate (HCons f _) x = case x of
                                    Left e  -> f (Left e)
                                    Right _ -> True

instance {-# OVERLAPPING #-} (ctx a)
    => HasPredicate (Constraint ctx ': xs) (Either ServantError a) where
    getPredicate (HConsC f _) x = case x of
        Left _  -> discard  -- Not clear whether checking for FailureResponse is better
        Right v -> getConstraint f v
    getPredicate (HCons _ _) _  = error "not impossible, but non-sensical"

instance {-# OVERLAPPABLE #-} (ls ~ (b ': xs), HasPredicate xs a)
    => HasPredicate ls a where
    getPredicate (HCons _ xs) = getPredicate xs
    getPredicate _ = error "impossible"

-- | Add a predicate to a list of predicates. Note that the predicate may not
-- be polymorphic.
addPredicate :: (a -> Bool) -> Predicates b -> Predicates (a ': b)
addPredicate = HCons

-- | Add a predicate with a class constraint.
--
-- Note that every possible argument must be an instance of that class for this
-- to typecheck. In other words, if the @Predicates@ is being used for return
-- types, every return type in the API must be an instance of the class. If
-- it's being used for filtering, every capture, header, body, etc. type must
-- be an instance of that class.
--
-- This can be used to for example test that returned JSON has certain
-- properties, or (via generics) that if any datatype contains a (possibly
-- nested) field of a particular type, it always meets certain properties.
addPolyPredicate :: proxy ctx -> (forall a. ctx a => a -> Bool) -> Predicates b
  -> Predicates (Constraint ctx ': b)
addPolyPredicate _ p = HConsC (Constraint p)

-- | Given a predicate over an @p :: a -> Bool@, add a predicate to the @Predicates@
-- list that succeeds on an @val :: Either ServantError a@ if @val@ is a
-- @Left@, or a @Right v@ such that @p a == True@.
addRightPredicate :: (a -> Bool) -> Predicates b -> Predicates (Either ServantError a ': b)
addRightPredicate p = addPredicate $ either (const True) p

-- | The @Left@ analog of 'addRightPredicate'.
addLeftPredicate :: (ServantError -> Bool) -> Predicates b
    -> Predicates (Either ServantError Void ': b)
addLeftPredicate p = addPredicate $ either p (error "impossible")

-- | An empty list of predicates. This doesn't discard any values when used as
-- a filter, and doesn't fail any value when used as a condition to satisfy.
emptyPredicates :: Predicates '[]
emptyPredicates = HNil

-- * Useful predicates

-- | A @Predicates@ list that fails a test if the response is an HTTP 500 error.
never500s :: Predicates '[Either ServantError Void]
never500s = addLeftPredicate go emptyPredicates
  where
   go (FailureResponse x _ _) = statusCode x /= 500
   go _ = True

-- | A @Predicates@ list that fails a test if the response is anything but a
-- top-level object (e.g., if it is an array or literal).
--
-- Returning anything other than object is considered bad practice, as
--
--   (1) it is hard to modify the returned value while maintaining backwards
--   compatibility;
--   (2) many older tools do not support top-level arrays;
--   (3) whether top-level numbers, booleans, or strings are valid JSON depends
--   on what RFC you're going by;
--   (4) there are security issues with top-level arrays.
onlyJsonObjects :: Predicates '[Constraint ToJSON]
onlyJsonObjects = addPolyPredicate (Proxy :: Proxy ToJSON) go emptyPredicates
  where
    go x = case toJSON x of
      Object _ -> True
      _        -> False
