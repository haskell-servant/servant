{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Authentication where

import           Data.ByteString (ByteString)
import           Data.Typeable   (Typeable)
import           GHC.TypeLits    (Symbol)

-- | we can be either Strict or Lax.
-- Strict: all handlers under 'AuthProtect' take a 'usr' argument.
--         when auth fails, we call user-supplied handlers to respond.
-- Lax: all handlers under 'AuthProtect' take a 'Maybe usr' argument.
--      when auth fails, we call the handlers with 'Nothing'.
data AuthPolicy = Strict | Lax

-- | the combinator to be used in API types
data AuthProtect authdata usr (policy :: AuthPolicy)

-- | what we'll ask user to provide at the server-level when we see a
-- 'AuthProtect' combinator in an API type
data family AuthProtected authdata usr subserver :: AuthPolicy -> *

-- | Basic Authentication with respect to a specified @realm@ and a @lookup@
-- type to encapsulate authentication logic.
data BasicAuth (realm :: Symbol) = BasicAuth { baUser :: ByteString
                                             , baPass :: ByteString
                                             } deriving (Eq, Show, Typeable)
