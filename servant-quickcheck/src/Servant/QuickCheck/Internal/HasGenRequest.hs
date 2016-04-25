{-# LANGUAGE PolyKinds #-}
module Servant.QuickCheck.Internal.HasGenRequest where

import           Data.Default.Class       (def)
import           Data.Monoid              ((<>))
import           Data.String              (fromString)
import           Data.String.Conversions  (cs)
import           GHC.TypeLits
import           Network.HTTP.Client      (Request, RequestBody (..), host,
                                           method, path, port, requestBody,
                                           requestHeaders, secure, queryString)
import           Network.HTTP.Media       (renderHeader)
import           Servant
import           Servant.API.ContentTypes
import           Servant.Client           (BaseUrl (..), Scheme (..))
import           Test.QuickCheck


class HasGenRequest a where
    genRequest :: Proxy a -> Gen (BaseUrl -> Request)

instance (HasGenRequest a, HasGenRequest b) => HasGenRequest (a :<|> b) where
    genRequest _
      = oneof [ genRequest (Proxy :: Proxy a)
              , genRequest (Proxy :: Proxy b)
              ]

instance (KnownSymbol path, HasGenRequest b) => HasGenRequest (path :> b) where
    genRequest _ = do
      old' <- old
      return $ \burl -> let r = old' burl in r { path = new <> "/" <> path r }
      where
        old = genRequest (Proxy :: Proxy b)
        new = cs $ symbolVal (Proxy :: Proxy path)

instance (Arbitrary c, HasGenRequest b, ToHttpApiData c )
    => HasGenRequest (Capture x c :> b) where
    genRequest _ = do
      old' <- old
      new' <- toUrlPiece <$> new
      return $ \burl -> let r = old' burl in r { path = cs new' <> "/" <> path r }
      where
        old = genRequest (Proxy :: Proxy b)
        new = arbitrary :: Gen c

instance (Arbitrary c, KnownSymbol h, HasGenRequest b, ToHttpApiData c)
    => HasGenRequest (Header h c :> b) where
    genRequest _ = do
      old' <- old
      new' <- toUrlPiece <$> new
      return $ \burl -> let r = old' burl in r {
          requestHeaders = (hdr, cs new') : requestHeaders r }
      where
        old = genRequest (Proxy :: Proxy b)
        hdr = fromString $ symbolVal (Proxy :: Proxy h)
        new = arbitrary :: Gen c

instance (AllMimeRender x c, Arbitrary c, HasGenRequest b)
    => HasGenRequest (ReqBody x c :> b) where
    genRequest _ = do
      old' <- old
      new' <- new
      (ct, bd) <- elements $ allMimeRender (Proxy :: Proxy x) new'
      return $ \burl -> let r = old' burl in r {
          requestBody = RequestBodyLBS bd
        , requestHeaders = ("Content-Type", renderHeader ct) : requestHeaders r
        }
      where
        old = genRequest (Proxy :: Proxy b)
        new = arbitrary :: Gen c

instance (KnownSymbol x, Arbitrary c, ToHttpApiData c, HasGenRequest b)
    => HasGenRequest (QueryParam x c :> b) where
    genRequest _ = do
      new' <- new
      old' <- old
      return $ \burl -> let r = old' burl in r {
          queryString = queryString r
                     <> param <> "=" <> cs (toQueryParam new') }
      where
        old = genRequest (Proxy :: Proxy b)
        param = cs $ symbolVal (Proxy :: Proxy x)
        new = arbitrary :: Gen c

instance (KnownSymbol x, Arbitrary c, ToHttpApiData c, HasGenRequest b)
    => HasGenRequest (QueryParams x c :> b) where
    genRequest _ = do
      new' <- new
      old' <- old
      return $ \burl -> let r = old' burl in r {
          queryString = queryString r
                     <> if length new' > 0 then fold (toParam <$> new') else ""}
      where
        old = genRequest (Proxy :: Proxy b)
        param = cs $ symbolVal (Proxy :: Proxy x)
        new = arbitrary :: Gen [c]
        toParam c = param <> "[]=" <> cs (toQueryParam c)
        fold = foldr1 (\a b -> a <> "&" <> b)

instance (KnownSymbol x, HasGenRequest b)
    => HasGenRequest (QueryFlag x :> b) where
    genRequest _ = do
      old' <- old
      return $ \burl -> let r = old' burl in r {
          queryString = queryString r <> param <> "=" }
      where
        old = genRequest (Proxy :: Proxy b)
        param = cs $ symbolVal (Proxy :: Proxy x)

instance (ReflectMethod method)
    => HasGenRequest (Verb (method :: k) (status :: Nat) (cts :: [*]) a) where
    genRequest _ = return $ \burl -> def
       { host = cs $ baseUrlHost burl
       , port = baseUrlPort burl
       , secure = baseUrlScheme burl == Https
       , method = reflectMethod (Proxy :: Proxy method)
       }

instance (HasGenRequest a) => HasGenRequest (RemoteHost :> a) where
    genRequest _ = genRequest (Proxy :: Proxy a)

instance (HasGenRequest a) => HasGenRequest (IsSecure :> a) where
    genRequest _ = genRequest (Proxy :: Proxy a)

instance (HasGenRequest a) => HasGenRequest (HttpVersion :> a) where
    genRequest _ = genRequest (Proxy :: Proxy a)

instance (HasGenRequest a) => HasGenRequest (Vault :> a) where
    genRequest _ = genRequest (Proxy :: Proxy a)

instance (HasGenRequest a) => HasGenRequest (WithNamedContext x y a) where
    genRequest _ = genRequest (Proxy :: Proxy a)

-- TODO: Try logging in
instance (HasGenRequest a) => HasGenRequest (BasicAuth x y :> a) where
    genRequest _ = genRequest (Proxy :: Proxy a)
