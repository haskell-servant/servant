{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Servant.Server.Internal.Before where

import           Data.Typeable
import           GHC.TypeLits                (KnownSymbol)
import           Network.HTTP.Types          hiding (Header, ResponseHeaders)
import           Servant.API                 ((:<|>) (..), (:>), Capture,
                                              Delete, Get, Header,
                                              IsSecure(..), Patch, Post, Put,
                                              QueryFlag, QueryParam, QueryParams,
                                              RemoteHost, ReqBody, Vault)
import           Servant.API.ContentTypes    (AllCTUnrender (..))
import           Servant.Server.Internal

import           Web.HttpApiData             (FromHttpApiData)


class HasBefore layout where
  before :: Monad m => Proxy layout -> m a -> (a -> ServerT layout m) -> ServerT layout m

instance (HasBefore a, HasBefore b) => HasBefore (a :<|> b) where
  before Proxy m f = before pa m (\a -> case f a of l :<|> _ -> l)
                :<|> before pb m (\a -> case f a of _ :<|> r -> r)
    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b

instance HasBefore (Delete ctypes a) where
  before Proxy = (>>=)

instance HasBefore (Get ctypes a) where
  before Proxy = (>>=)

instance HasBefore (Post ctypes a) where
  before Proxy = (>>=)

instance HasBefore (Put ctypes a) where
  before Proxy = (>>=)

instance HasBefore (Patch ctypes a) where
  before Proxy = (>>=)

instance (KnownSymbol path, HasBefore sublayout) => HasBefore (path :> sublayout) where
  before Proxy = before (Proxy :: Proxy sublayout)

instance (KnownSymbol capture, FromHttpApiData a, HasBefore sublayout)
      => HasBefore (Capture capture a :> sublayout) where
  before Proxy m f x = before (Proxy :: Proxy sublayout) m (`f` x)

instance (HasBefore sublayout) => HasBefore (Header sym a :> sublayout) where
  before Proxy m f x = before (Proxy :: Proxy sublayout) m (`f` x)

instance (KnownSymbol sym, FromHttpApiData a, HasBefore sublayout)
      => HasBefore (QueryParam sym a :> sublayout) where
  before Proxy m f x = before (Proxy :: Proxy sublayout) m (`f` x)

instance (KnownSymbol sym, FromHttpApiData a, HasBefore sublayout)
      => HasBefore (QueryParams sym a :> sublayout) where
  before Proxy m f x = before (Proxy :: Proxy sublayout) m (`f` x)

instance (HasBefore sublayout) => HasBefore (QueryFlag sym :> sublayout) where
  before Proxy m f x = before (Proxy :: Proxy sublayout) m (`f` x)

instance (AllCTUnrender list a, HasBefore sublayout) => HasBefore (ReqBody list a :> sublayout) where
  before Proxy m f x = before (Proxy :: Proxy sublayout) m (`f` x)

instance HasBefore api => HasBefore (RemoteHost :> api) where
  before Proxy m f x = before (Proxy :: Proxy api) m (`f` x)

instance HasBefore api => HasBefore (IsSecure :> api) where
  before Proxy m f x = before (Proxy :: Proxy api) m (`f` x)

instance HasBefore api => HasBefore (Vault :> api) where
  before Proxy m f x = before (Proxy :: Proxy api) m (`f` x)

instance HasBefore api => HasBefore (HttpVersion :> api) where
  before Proxy m f x = before (Proxy :: Proxy api) m (`f` x)
