{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE UndecidableInstances     #-}
{-# LANGUAGE UndecidableSuperClasses  #-}

{-|
This module collects utilities for manipulating @servant@ API types. The
functionality in this module is for advanced usage.

The code samples in this module use the following type synonym:

> type SampleAPI = "hello" :> Get '[JSON] Int
>             :<|> "bye" :> Capture "name" String :> Post '[JSON, PlainText] Bool

-}
module Servant.API.TypeLevel (
    -- $setup
    -- * API predicates
    Endpoints,
    -- ** Lax inclusion
    IsElem',
    IsElem,
    IsSubAPI,
    AllIsElem,
    -- ** Strict inclusion
    IsIn,
    IsStrictSubAPI,
    AllIsIn,
    -- * Helpers
    -- ** Lists
    MapSub,
    AppendList,
    IsSubList,
    Elem,
    ElemGo,
    -- ** Logic
    Or,
    And,
    -- ** Fragment
    FragmentUnique,
    AtLeastOneFragment
    ) where


import           GHC.Exts
                 (Constraint)
import           Servant.API.Alternative
                 (type (:<|>))
import           Servant.API.Capture
                 (Capture, CaptureAll)
import           Servant.API.Fragment
import           Servant.API.Header
                 (Header)
import           Servant.API.QueryParam
                 (QueryFlag, QueryParam, QueryParams)
import           Servant.API.ReqBody
                 (ReqBody)
import           Servant.API.Sub
                 (type (:>))
import           Servant.API.Verbs
                 (Verb)
import           Servant.API.UVerb
                 (UVerb)
import           GHC.TypeLits
                 (ErrorMessage (..), TypeError)



-- * API predicates

-- | Flatten API into a list of endpoints.
--
-- >>> Refl :: Endpoints SampleAPI :~: '["hello" :> Verb 'GET 200 '[JSON] Int, "bye" :> (Capture "name" String :> Verb 'POST 200 '[JSON, PlainText] Bool)]
-- Refl
type family Endpoints api where
  Endpoints (a :<|> b) = AppendList (Endpoints a) (Endpoints b)
  Endpoints (e :> a)   = MapSub e (Endpoints a)
  Endpoints a = '[a]

-- ** Lax inclusion

-- | You may use this type family to tell the type checker that your custom
-- type may be skipped as part of a link. This is useful for things like
-- @'QueryParam'@ that are optional in a URI and do not affect them if they are
-- omitted.
--
-- >>> data CustomThing
-- >>> type instance IsElem' e (CustomThing :> s) = IsElem e s
--
-- Note that @'IsElem'@ is called, which will mutually recurse back to @'IsElem''@
-- if it exhausts all other options again.
--
-- Once you have written a @HasLink@ instance for @CustomThing@ you are ready to go.
type family IsElem' a s :: Constraint

-- | Closed type family, check if @endpoint@ is within @api@.
-- Uses @'IsElem''@ if it exhausts all other options.
--
-- >>> ok (Proxy :: Proxy (IsElem ("hello" :> Get '[JSON] Int) SampleAPI))
-- OK
--
-- >>> ok (Proxy :: Proxy (IsElem ("bye" :> Get '[JSON] Int) SampleAPI))
-- ...
-- ... Could not deduce...
-- ...
--
-- An endpoint is considered within an api even if it is missing combinators
-- that don't affect the URL:
--
-- >>> ok (Proxy :: Proxy (IsElem (Get '[JSON] Int) (Header "h" Bool :> Get '[JSON] Int)))
-- OK
--
-- >>> ok (Proxy :: Proxy (IsElem (Get '[JSON] Int) (ReqBody '[JSON] Bool :> Get '[JSON] Int)))
-- OK
--
-- *N.B.:* @IsElem a b@ can be seen as capturing the notion of whether the URL
-- represented by @a@ would match the URL represented by @b@, *not* whether a
-- request represented by @a@ matches the endpoints serving @b@ (for the
-- latter, use 'IsIn').
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
  IsElem sa (Fragment x :> sb)            = IsElem sa sb
  IsElem (Verb m s ct typ) (Verb m s ct' typ)
                                          = IsSubList ct ct'
  IsElem e e                              = ()
  IsElem e a                              = IsElem' e a

-- | Check whether @sub@ is a sub-API of @api@.
--
-- >>> ok (Proxy :: Proxy (IsSubAPI SampleAPI (SampleAPI :<|> Get '[JSON] Int)))
-- OK
--
-- >>> ok (Proxy :: Proxy (IsSubAPI (SampleAPI :<|> Get '[JSON] Int) SampleAPI))
-- ...
-- ... Could not deduce...
-- ...
--
-- This uses @IsElem@ for checking; thus the note there applies here.
type family IsSubAPI sub api :: Constraint where
  IsSubAPI sub api = AllIsElem (Endpoints sub) api

-- | Check that every element of @xs@ is an endpoint of @api@ (using @'IsElem'@).
type family AllIsElem xs api :: Constraint where
  AllIsElem '[] api = ()
  AllIsElem (x ': xs) api = (IsElem x api, AllIsElem xs api)

-- ** Strict inclusion

-- | Closed type family, check if @endpoint@ is exactly within @api@.
--
-- >>> ok (Proxy :: Proxy (IsIn ("hello" :> Get '[JSON] Int) SampleAPI))
-- OK
--
-- Unlike 'IsElem', this requires an *exact* match.
--
-- >>> ok (Proxy :: Proxy (IsIn (Get '[JSON] Int) (Header "h" Bool :> Get '[JSON] Int)))
-- ...
-- ... Could not deduce...
-- ...
type family IsIn (endpoint :: *) (api :: *) :: Constraint where
  IsIn e (sa :<|> sb)                = Or (IsIn e sa) (IsIn e sb)
  IsIn (e :> sa) (e :> sb)           = IsIn sa sb
  IsIn e e                           = ()

-- | Check whether @sub@ is a sub API of @api@.
--
-- Like 'IsSubAPI', but uses 'IsIn' rather than 'IsElem'.
type family IsStrictSubAPI sub api :: Constraint where
  IsStrictSubAPI sub api = AllIsIn (Endpoints sub) api

-- | Check that every element of @xs@ is an endpoint of @api@ (using @'IsIn'@).
--
-- >>> ok (Proxy :: Proxy (AllIsIn (Endpoints SampleAPI) SampleAPI))
-- OK
type family AllIsIn xs api :: Constraint where
  AllIsIn '[] api = ()
  AllIsIn (x ': xs) api = (IsIn x api, AllIsIn xs api)

-- * Helpers

-- ** Lists

-- | Apply @(e :>)@ to every API in @xs@.
type family MapSub e xs where
  MapSub e '[] = '[]
  MapSub e (x ': xs) = (e :> x) ': MapSub e xs

-- | Append two type-level lists.
type family AppendList xs ys where
  AppendList '[]       ys = ys
  AppendList (x ': xs) ys = x ': AppendList xs ys

type family IsSubList a b :: Constraint where
  IsSubList '[] b          = ()
  IsSubList (x ': xs) y    = Elem x y `And` IsSubList xs y

-- | Check that a value is an element of a list:
--
-- >>> ok (Proxy :: Proxy (Elem Bool '[Int, Bool]))
-- OK
--
-- >>> ok (Proxy :: Proxy (Elem String '[Int, Bool]))
-- ...
-- ... [Char]...'[Int, Bool...
-- ...
type Elem e es = ElemGo e es es

-- 'orig' is used to store original list for better error messages
type family ElemGo e es orig :: Constraint where
  ElemGo x (x ': xs) orig = ()
  ElemGo y (x ': xs) orig = ElemGo y xs orig
  -- Note [Custom Errors]
  ElemGo x '[] orig       = TypeError ('ShowType x
                                 ':<>: 'Text " expected in list "
                                 ':<>: 'ShowType orig)

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

{- Note [Custom Errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We might try to factor these our more cleanly, but the type synonyms and type
families are not evaluated (see https://ghc.haskell.org/trac/ghc/ticket/12048).
-}

-- ** Fragment

class FragmentUnique api => AtLeastOneFragment api

-- | If fragment appeared in API endpoint twice, compile-time error would be raised.
--
-- >>> -- type FailAPI = Fragment Bool :> Fragment Int :> Get '[JSON] NoContent
-- >>> instance AtLeastOneFragment FailAPI
-- ...
-- ...Only one Fragment allowed per endpoint in api...
-- ...
-- ...In the instance declaration for...
instance AtLeastOneFragment (Verb m s ct typ)

instance AtLeastOneFragment (UVerb m cts as)

instance AtLeastOneFragment (Fragment a)

type family FragmentUnique api :: Constraint where
  FragmentUnique (sa :<|> sb)       = And (FragmentUnique sa) (FragmentUnique sb)
  FragmentUnique (Fragment a :> sa) = FragmentNotIn sa (Fragment a :> sa)
  FragmentUnique (x :> sa)          = FragmentUnique sa
  FragmentUnique (Fragment a)       = ()
  FragmentUnique x                  = ()

type family FragmentNotIn api orig :: Constraint where
  FragmentNotIn (sa :<|> sb)       orig =
    And (FragmentNotIn sa orig) (FragmentNotIn sb orig)
  FragmentNotIn (Fragment c :> sa) orig = TypeError (NotUniqueFragmentInApi orig)
  FragmentNotIn (x :> sa)          orig = FragmentNotIn sa orig
  FragmentNotIn (Fragment c)       orig = TypeError (NotUniqueFragmentInApi orig)
  FragmentNotIn x                  orig = ()

type NotUniqueFragmentInApi api =
    'Text "Only one Fragment allowed per endpoint in api ‘"
    ':<>: 'ShowType api
    ':<>: 'Text "’."

-- $setup
--
-- The doctests in this module are run with following preamble:
--
-- >>> :set -XPolyKinds
-- >>> :set -XGADTs
-- >>> :set -XTypeSynonymInstances -XFlexibleInstances
-- >>> import Data.Proxy
-- >>> import Data.Type.Equality
-- >>> import Servant.API
-- >>> data OK ctx where OK :: ctx => OK ctx
-- >>> instance Show (OK ctx) where show _ = "OK"
-- >>> let ok :: ctx => Proxy ctx -> OK ctx; ok _ = OK
-- >>> type SampleAPI = "hello" :> Get '[JSON] Int :<|> "bye" :> Capture "name" String :> Post '[JSON, PlainText] Bool
-- >>> type FailAPI = Fragment Bool :> Fragment Int :> Get '[JSON] NoContent
-- >>> let sampleAPI = Proxy :: Proxy SampleAPI
