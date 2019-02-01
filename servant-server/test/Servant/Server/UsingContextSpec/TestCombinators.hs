{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | These are custom combinators for Servant.Server.UsingContextSpec.
--
-- (For writing your own combinators you need to import Internal modules, for
-- just *using* combinators that require a Context, you don't. This module is
-- separate from Servant.Server.UsingContextSpec to test that the module imports
-- work out this way.)
module Servant.Server.UsingContextSpec.TestCombinators where

import           GHC.TypeLits

import           Servant

data ExtractFromContext

instance (HasContextEntry context String, HasServer subApi context) =>
  HasServer (ExtractFromContext :> subApi) context where

  type ServerT (ExtractFromContext :> subApi) m =
    String -> ServerT subApi m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy subApi) pc nt . s

  route Proxy context delayed =
    route subProxy context (fmap inject delayed)
    where
      subProxy :: Proxy subApi
      subProxy = Proxy

      inject f = f (getContextEntry context)

data InjectIntoContext

instance (HasServer subApi (String ': context)) =>
  HasServer (InjectIntoContext :> subApi) context where

  type ServerT (InjectIntoContext :> subApi) m =
    ServerT subApi m

  hoistServerWithContext _ _ nt s =
    hoistServerWithContext (Proxy :: Proxy subApi) (Proxy :: Proxy (String ': context)) nt s

  route Proxy context delayed =
    route subProxy newContext delayed
    where
      subProxy :: Proxy subApi
      subProxy = Proxy

      newContext = ("injected" :: String) :. context

data NamedContextWithBirdface (name :: Symbol) (subContext :: [*])

instance (HasContextEntry context (NamedContext name subContext), HasServer subApi subContext) =>
  HasServer (NamedContextWithBirdface name subContext :> subApi) context where

  type ServerT (NamedContextWithBirdface name subContext :> subApi) m =
    ServerT subApi m

  hoistServerWithContext _ _ nt s =
    hoistServerWithContext (Proxy :: Proxy subApi) (Proxy :: Proxy subContext) nt s

  route Proxy context delayed =
    route subProxy subContext delayed
    where
      subProxy :: Proxy subApi
      subProxy = Proxy

      subContext :: Context subContext
      subContext = descendIntoNamedContext (Proxy :: Proxy name) context
