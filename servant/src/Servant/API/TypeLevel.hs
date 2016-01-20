{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.API.TypeLevel where

import GHC.Exts(Constraint)
import Servant.API.Capture ( Capture )
import Servant.API.ReqBody ( ReqBody )
import Servant.API.QueryParam ( QueryParam, QueryParams, QueryFlag )
import Servant.API.Header ( Header )
import Servant.API.Verbs ( Verb )
import Servant.API.Sub ( type (:>) )
import Servant.API.Alternative ( type (:<|>) )

-- * API predicates

-- | You may use this type family to tell the type checker that your custom
-- type may be skipped as part of a link. This is useful for things like
-- 'QueryParam' that are optional in a URI and do not affect them if they are
-- omitted.
--
-- >>> data CustomThing
-- >>> type instance IsElem' e (CustomThing :> s) = IsElem e s
--
-- Note that 'IsElem' is called, which will mutually recurse back to `IsElem'`
-- if it exhausts all other options again.
--
-- Once you have written a HasLink instance for CustomThing you are ready to
-- go.
type family IsElem' a s :: Constraint

-- | Closed type family, check if @endpoint@ is within @api@.
-- Uses @'IsElem''@.
type family IsElem endpoint api :: Constraint where
    IsElem e (sa :<|> sb)                   = Or (IsElem e sa) (IsElem e sb)
    IsElem (e :> sa) (e :> sb)              = IsElem sa sb
    IsElem sa (Header sym x :> sb)          = IsElem sa sb
    IsElem sa (ReqBody y x :> sb)           = IsElem sa sb
    IsElem (CaptureAll z y :> sa) (CaptureAll x y :> sb)
                                            = IsElem sa sb
    IsElem (Capture z y :> sa) (Capture x y :> sb)
                                            = IsElem sa sb
    IsElem sa (QueryParam x y :> sb)        = IsElem sa sb
    IsElem sa (QueryParams x y :> sb)       = IsElem sa sb
    IsElem sa (QueryFlag x :> sb)           = IsElem sa sb
    IsElem (Verb m s ct typ) (Verb m s ct' typ)
                                            = IsSubList ct ct'
    IsElem e e                              = ()
    IsElem e a                              = IsElem' e a

-- * Helpers

-- ** Lists

type family IsSubList a b :: Constraint where
    IsSubList '[] b          = ()
    IsSubList (x ': xs) y    = Elem x y `And` IsSubList xs y

type family Elem e es :: Constraint where
    Elem x (x ': xs) = ()
    Elem y (x ': xs) = Elem y xs

-- ** Logic

-- | If either a or b produce an empty constraint, produce an empty constraint.
type family Or (a :: Constraint) (b :: Constraint) :: Constraint where
    -- This works because of:
    -- https://ghc.haskell.org/trac/ghc/wiki/NewAxioms/CoincidentOverlap
    Or () b       = ()
    Or a ()       = ()

-- | If both a or b produce an empty constraint, produce an empty constraint.
type family And (a :: Constraint) (b :: Constraint) :: Constraint where
    And () ()     = ()

