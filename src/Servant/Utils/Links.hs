{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type safe internal links.
--
-- Provides the function 'mkLink':
--
-- @
--   {-# LANGUAGE DataKinds #-}
--   {-# LANGUAGE TypeFamilies #-}
--   {-# LANGUAGE TypeOperators #-}
--
--   import Servant.API
--   import Servant.Utils.Links
--
--   -- You might want to put some custom types in your API
--   data Thing = Thing
--
--   -- If you want these to form part of a valid URL, just add them to the
--   -- open type family IsElem' like so:
--   type instance IsElem' e (Thing :> s) = IsElem e s
--
--   type API =  "hello" :> Thing :> Get Int
--          :<|> "bye"   :> QueryParam "name" String :> Post Bool
--
--   api :: API
--   api = undefined
--
--   link1 :: "hello" :> Get Int
--   link1 = undefined
--
--   link2 :: "hello" :> Delete
--   link2 = undefined
--
--   main :: IO ()
--   main =
--       --  typechecks, prints "/hello"'
--       let Link str = mkLink link1 api
--       in putStrLn str
--
--       -- doesn't typecheck
--       -- mkLink link2  api
-- @
--
-- That is, 'mkLink' takes two arguments, a link and a sitemap, and
-- returns a 'Link', but only typechecks if the link proxy is a valid link,
-- and part of the sitemap.
--
-- __N.B.:__ 'mkLink' assumes a capture matches any string (without slashes).
module Servant.Utils.Links (
    -- * Link and mkLink
    -- | The only end-user utilities
      mkLink
    , Link, unLink
    -- * Internal
    -- | These functions will likely only be of interest if you are writing
    -- more API combinators and would like to extend the behavior of
    -- 'mkLink'
    , ValidLinkIn()
    , VLinkHelper(..)
    , IsElem
    , IsElem'
    , IsLink
                           ) where

import Data.Proxy ( Proxy(..) )
import GHC.TypeLits ( KnownSymbol, Symbol, symbolVal )
import GHC.Exts(Constraint)

import Servant.API.Capture ( Capture )
import Servant.API.ReqBody ( ReqBody )
import Servant.API.QueryParam ( QueryParam, QueryParams, QueryFlag )
import Servant.API.MatrixParam ( MatrixParam, MatrixParams, MatrixFlag )
import Servant.API.Get ( Get )
import Servant.API.Post ( Post )
import Servant.API.Put ( Put )
import Servant.API.Delete ( Delete )
import Servant.API.Sub ( type (:>) )
import Servant.API.Raw ( Raw )
import Servant.API.Alternative ( type (:<|>) )


type family Or (a :: Constraint) (b :: Constraint) :: Constraint where
    Or () b       = ()
    Or a ()       = ()

type family And (a :: Constraint)  (b :: Constraint) :: Constraint where
    And () () = ()

type family IsElem' a s :: Constraint

type family IsElem a s :: Constraint where
    IsElem e (sa :<|> sb)                = Or (IsElem e sa) (IsElem e sb)
    IsElem (e :> sa) (e :> sb)           = IsElem sa sb
    IsElem (e :> sa) (Capture x y :> sb) = IsElem sa sb
    IsElem sa (ReqBody x :> sb)          = IsElem sa sb
    IsElem sa (QueryParam x y :> sb)     = IsElem sa sb
    IsElem sa (QueryParams x y :> sb)    = IsElem sa sb
    IsElem sa (QueryFlag x :> sb)        = IsElem sa sb
    IsElem sa (MatrixParam x y :> sb)    = IsElem sa sb
    IsElem sa (MatrixParams x y :> sb)   = IsElem sa sb
    IsElem sa (MatrixFlag x :> sb)       = IsElem sa sb
    IsElem e e                           = ()
    IsElem e a                           = IsElem' e a

type family IsLink'' l :: Constraint where
    IsLink'' (e :> Get x)    = IsLink' e
    IsLink'' (e :> Post x)   = IsLink' e
    IsLink'' (e :> Put x)    = IsLink' e
    IsLink'' (e :> Delete)   = IsLink' e
    IsLink'' (e :> Raw)      = IsLink' e

type family IsLink' e :: Constraint where
    IsLink' (f :: Symbol)  = ()

type family IsLink e :: Constraint where
    IsLink (a :> b)        = Or (And (IsLink' a) (IsLink'' b))
                                (IsLink'' (a :> b))


-- | The 'ValidLinkIn f s' constraint holds when 's' is an API that
-- contains 'f', and 'f' is a link.
class ValidLinkIn f s where
    mkLink :: f -> s -> Link  -- ^ This function will only typecheck if `f`
                              -- is an URI within `s`

instance ( IsElem f s 
         , IsLink f
         , VLinkHelper f) => ValidLinkIn f s where
    mkLink _ _ = Link (vlh (Proxy :: Proxy f))

-- | A safe link datatype.
-- The only way of constructing a 'Link' is using 'mkLink', which means any
-- 'Link' is guaranteed to be part of the mentioned API.
newtype Link = Link { unLink :: String } deriving Show

class VLinkHelper f where
    vlh :: forall proxy. proxy f -> String

instance (KnownSymbol s, VLinkHelper e) => VLinkHelper (s :> e) where
    vlh _ = "/" ++ symbolVal (Proxy :: Proxy s) ++ vlh (Proxy :: Proxy e)

instance VLinkHelper (Get x) where
    vlh _ = ""

instance VLinkHelper (Post x) where
    vlh _ = ""
