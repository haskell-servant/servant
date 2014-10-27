{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.API.Elem where

import Data.Proxy
import GHC.TypeLits

import Servant.API.Capture
import Servant.API.ReqBody
import Servant.API.QueryParam
import Servant.API.Get
import Servant.API.Post
import Servant.API.Put
import Servant.API.Delete
import Servant.API.Sub
import Servant.API.Alternative


type family Or a b where
    Or 'False 'False = 'False
    Or 'True b       = 'True
    Or a 'True       = 'True

type family And a b where
    And 'True 'True = 'True
    And a 'False    = 'False
    And 'False b    = 'False

type family IsElem a s where
    IsElem e (sa :<|> sb)                = Or (IsElem e sa) (IsElem e sb)
    IsElem (e :> sa) (e :> sb)           = IsElem sa sb
    IsElem (e :> sa) (Capture x y :> sb) = IsElem sa sb
    IsElem sa (ReqBody x :> sb)          = IsElem sa sb
    IsElem sa (QueryParam x y :> sb)     = IsElem sa sb
    IsElem e e                           = 'True
    IsElem e a                           = 'False

type family IsLink'' l where
    IsLink'' (e :> Get x)    = IsLink' e
    IsLink'' (e :> Post x)   = IsLink' e
    IsLink'' (e :> Put x)    = IsLink' e
    IsLink'' (e :> Delete)   = IsLink' e
    IsLink'' a               = 'False

type family IsLink' e where
    IsLink' (f :: Symbol)  = 'True

type family IsLink e where
    IsLink (a :> b)        = Or (And (IsLink' a) (IsLink'' b))
                                (IsLink'' (a :> b))


-- | The 'ValidLinkIn f s' constraint holds when 's' is an API that
-- contains 'f', and 'f' is a link.
class ValidLinkIn f s where
    mkLink :: f -> s -> Link  -- ^ This function will only typecheck if `f`
                              -- is an URI within `s`

instance ( IsElem f s ~ 'True
         , IsLink f ~ 'True
         , VLinkHelper f) => ValidLinkIn f s where
    mkLink _ _ = Link (vlh (Proxy :: Proxy f))

data Link = Link String deriving Show

class VLinkHelper f where
    vlh :: forall proxy. proxy f -> String

instance (KnownSymbol s, VLinkHelper e) => VLinkHelper (s :> e) where
    vlh _ = "/" ++ symbolVal (Proxy :: Proxy s) ++ vlh (Proxy :: Proxy e)

instance VLinkHelper (Get x) where
    vlh _ = ""

instance VLinkHelper (Post x) where
    vlh _ = ""

