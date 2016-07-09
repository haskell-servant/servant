{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Servant.JSSpec.CustomHeaders where

import           Control.Lens
import           Data.Monoid
import           Data.Proxy
import           Data.Text (pack)
import           GHC.TypeLits
import           Servant.JS.Internal

-- | This is a hypothetical combinator that fetches an Authorization header.
-- The symbol in the header denotes what kind of authentication we are
-- using -- Basic, Digest, whatever.
data Authorization (sym :: Symbol) a

instance (KnownSymbol sym, HasForeign lang () api)
    => HasForeign lang () (Authorization sym a :> api) where
    type Foreign () (Authorization sym a :> api) = Foreign () api

    foreignFor lang ftype Proxy req = foreignFor lang ftype (Proxy :: Proxy api) $
        req & reqHeaders <>~
          [ ReplaceHeaderArg (Arg "Authorization" ())
          $ tokenType (pack . symbolVal $ (Proxy :: Proxy sym)) ]
      where
        tokenType t = t <> " {Authorization}"

-- | This is a combinator that fetches an X-MyLovelyHorse header.
data MyLovelyHorse a

instance (HasForeign lang () api)
    => HasForeign lang () (MyLovelyHorse a :> api) where
    type Foreign () (MyLovelyHorse a :> api) = Foreign () api

    foreignFor lang ftype Proxy req = foreignFor lang ftype (Proxy :: Proxy api) $
        req & reqHeaders <>~ [ ReplaceHeaderArg (Arg "X-MyLovelyHorse" ()) tpl ]
      where
        tpl = "I am good friends with {X-MyLovelyHorse}"

-- | This is a combinator that fetches an X-WhatsForDinner header.
data WhatsForDinner a

instance (HasForeign lang () api)
    => HasForeign lang () (WhatsForDinner a :> api) where
    type Foreign () (WhatsForDinner a :> api) = Foreign () api

    foreignFor lang ftype Proxy req = foreignFor lang ftype (Proxy :: Proxy api) $
        req & reqHeaders <>~ [ ReplaceHeaderArg (Arg "X-WhatsForDinner" ()) tpl ]
      where
        tpl = "I would like {X-WhatsForDinner} with a cherry on top."
