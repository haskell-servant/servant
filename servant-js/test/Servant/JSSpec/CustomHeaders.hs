{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Servant.JSSpec.CustomHeaders where

import           Control.Lens
import           Data.Monoid
import           Data.Proxy
import           GHC.TypeLits
import           Servant.API
import           Servant.JS
import           Servant.JS.Internal

-- | This is a hypothetical combinator that fetches an Authorization header.
-- The symbol in the header denotes what kind of authentication we are
-- using -- Basic, Digest, whatever.
data Authorization (sym :: Symbol) a

instance (KnownSymbol sym, HasJS sublayout)
    => HasJS (Authorization sym a :> sublayout) where
    type JS (Authorization sym a :> sublayout) = JS sublayout

    javascriptFor Proxy req = javascriptFor (Proxy :: Proxy sublayout) $
        req & reqHeaders <>~ [ ReplaceHeaderArg "Authorization" $
                               tokenType (symbolVal (Proxy :: Proxy sym)) ]
      where
        tokenType t = t <> " {Authorization}"

-- | This is a combinator that fetches an X-MyLovelyHorse header.
data MyLovelyHorse a

instance (HasJS sublayout)
    => HasJS (MyLovelyHorse a :> sublayout) where
    type JS (MyLovelyHorse a :> sublayout) = JS sublayout

    javascriptFor Proxy req = javascriptFor (Proxy :: Proxy sublayout) $
        req & reqHeaders <>~ [ ReplaceHeaderArg "X-MyLovelyHorse" tpl ]
      where
        tpl = "I am good friends with {X-MyLovelyHorse}"

-- | This is a combinator that fetches an X-WhatsForDinner header.
data WhatsForDinner a

instance (HasJS sublayout)
    => HasJS (WhatsForDinner a :> sublayout) where
    type JS (WhatsForDinner a :> sublayout) = JS sublayout

    javascriptFor Proxy req = javascriptFor (Proxy :: Proxy sublayout) $
        req & reqHeaders <>~ [ ReplaceHeaderArg "X-WhatsForDinner" tpl ]
      where
        tpl = "I would like {X-WhatsForDinner} with a cherry on top."
