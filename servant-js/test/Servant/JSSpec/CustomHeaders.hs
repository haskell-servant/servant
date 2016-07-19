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
import           Servant.API.ContentTypes
import           Servant.JS.Internal

-- | This is a hypothetical combinator that fetches an Authorization header.
-- The symbol in the header denotes what kind of authentication we are
-- using -- Basic, Digest, whatever.
data Authorization (sym :: Symbol) a

instance (KnownSymbol sym, HasForeign lang NoContent api)
    => HasForeign lang NoContent (Authorization sym a :> api) where
    type Foreign NoContent (Authorization sym a :> api) = Foreign NoContent api

    foreignFor lang ftype Proxy req = foreignFor lang ftype (Proxy :: Proxy api) $
        req & reqHeaders <>~
          [ ReplaceHeaderArg (Arg "Authorization" NoContent)
          $ tokenType (pack . symbolVal $ (Proxy :: Proxy sym)) ]
      where
        tokenType t = t <> " {Authorization}"

-- | This is a combinator that fetches an X-MyLovelyHorse header.
data MyLovelyHorse a

instance (HasForeign lang NoContent api)
    => HasForeign lang NoContent (MyLovelyHorse a :> api) where
    type Foreign NoContent (MyLovelyHorse a :> api) = Foreign NoContent api

    foreignFor lang ftype Proxy req = foreignFor lang ftype (Proxy :: Proxy api) $
        req & reqHeaders <>~ [ ReplaceHeaderArg (Arg "X-MyLovelyHorse" NoContent) tpl ]
      where
        tpl = "I am good friends with {X-MyLovelyHorse}"

-- | This is a combinator that fetches an X-WhatsForDinner header.
data WhatsForDinner a

instance (HasForeign lang NoContent api)
    => HasForeign lang NoContent (WhatsForDinner a :> api) where
    type Foreign NoContent (WhatsForDinner a :> api) = Foreign NoContent api

    foreignFor lang ftype Proxy req = foreignFor lang ftype (Proxy :: Proxy api) $
        req & reqHeaders <>~ [ ReplaceHeaderArg (Arg "X-WhatsForDinner" NoContent) tpl ]
      where
        tpl = "I would like {X-WhatsForDinner} with a cherry on top."
