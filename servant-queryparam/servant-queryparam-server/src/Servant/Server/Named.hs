{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module exports orphan instances to make
-- [@servant-queryparam-core@](https://hackage.haskell.org/package/servant-queryparam-core) work with servers.
module Servant.Server.Named () where

import Data.Proxy
import qualified Data.Text as Text
import GHC.TypeLits
import Named
import Servant.API
import Servant.API.Modifiers
import Servant.Named
import Servant.Server

instance
  ( KnownSymbol sym
  , FromHttpApiData a
  , HasServer api context
  , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
  ) =>
  HasServer (NamedQueryParams sym a :> api) context
  where
  type
    ServerT (NamedQueryParams sym a :> api) m =
      sym :? [a] -> ServerT api m

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver =
    route (Proxy :: Proxy (QueryParams sym a :> api)) context $
      fmap (\f x -> f (ArgF $ Just x)) subserver

instance
  ( KnownSymbol sym
  , FromHttpApiData a
  , HasServer api context
  , SBoolI (FoldRequired mods)
  , SBoolI (FoldLenient mods)
  , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
  ) =>
  HasServer (NamedQueryParam' mods sym a :> api) context
  where
  type
    ServerT (NamedQueryParam' mods sym a :> api) m =
      If
        (FoldRequired mods)
        (If (FoldLenient mods) (sym :! Either Text.Text a) (sym :! a))
        (If (FoldLenient mods) (sym :? Either Text.Text a) (sym :? a)) ->
      ServerT api m

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver =
    route (Proxy :: Proxy (QueryParam' mods sym a :> api)) context $
      fmap
        ( \f x ->
            case sbool :: SBool (FoldRequired mods) of
              STrue -> case sbool :: SBool (FoldLenient mods) of
                STrue -> f (Arg x)
                SFalse -> f (Arg x)
              SFalse -> case sbool :: SBool (FoldLenient mods) of
                STrue -> f (ArgF x)
                SFalse -> f (ArgF x)
        )
        subserver

instance
  (KnownSymbol sym, HasServer api context) =>
  HasServer (NamedQueryFlag sym :> api) context
  where
  type
    ServerT (NamedQueryFlag sym :> api) m =
      sym :? Bool -> ServerT api m

  hoistServerWithContext _ pc nt s =
    hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver =
    route (Proxy :: Proxy (QueryFlag sym :> api)) context $
      fmap (\f -> f . ArgF . Just) subserver
