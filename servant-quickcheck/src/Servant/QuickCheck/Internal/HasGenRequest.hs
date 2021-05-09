{-# LANGUAGE CPP       #-}
{-# LANGUAGE PolyKinds #-}
module Servant.QuickCheck.Internal.HasGenRequest where

import Data.String              (fromString)
import Data.String.Conversions  (cs)
import GHC.TypeLits             (KnownSymbol, Nat, symbolVal)
import Network.HTTP.Client      (Request, RequestBody (..), host, method, path,
                                 port, queryString, requestBody, requestHeaders,
                                 secure, defaultRequest)
import Network.HTTP.Media       (renderHeader)
import Prelude.Compat
import Servant
import Servant.API.ContentTypes (AllMimeRender (..))
import Servant.Client           (BaseUrl (..), Scheme (..))
import Test.QuickCheck          (Arbitrary (..), Gen, elements, frequency)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (c2w)


-- -----------------------------------------------------------------------------
-- runGenRequest

-- | This function returns a QuickCheck `Gen a` when passed a servant API value,
-- typically a `Proxy API`. The generator returned is a function
-- that accepts a `BaseUrl` and returns a `Request`, which can then be used
-- to issue network requests. This `Gen` type makes it easier to compare distinct
-- APIs across different `BaseUrl`s.
runGenRequest :: HasGenRequest a => Proxy a -> Gen (BaseUrl -> Request)
runGenRequest = snd . genRequest


-- -----------------------------------------------------------------------------
-- HasGenRequest

-- | This is the core Servant-Quickcheck generator, which, when given a `Proxy API`
-- will return a pair of `Int` and `Gen a`, where `a` is a function from
-- `BaseUrl` to a `Network.Http.Client.Request`. The `Int` is a weight for the
-- QuickCheck `frequency` function which ensures a random distribution across
-- all endpoints in an API.
class HasGenRequest a where
    genRequest :: Proxy a -> (Int, Gen (BaseUrl -> Request))


instance (HasGenRequest a, HasGenRequest b) => HasGenRequest (a :<|> b) where
    genRequest _
      = (lf + rf, frequency [l, r])
      where
        l@(lf, _) = genRequest (Proxy :: Proxy a)
        r@(rf, _) = genRequest (Proxy :: Proxy b)


instance (KnownSymbol path, HasGenRequest b) => HasGenRequest (path :> b) where
    genRequest _ = (oldf, do
      old' <- old
      return $ \burl -> let r = old' burl
                            oldPath = path r
                            oldPath' = BS.dropWhile (== BS.c2w '/') oldPath
                            paths = filter (not . BS.null) [new, oldPath']
                            in r { path = "/" <> BS.intercalate "/" paths })
      where
        (oldf, old) = genRequest (Proxy :: Proxy b)
        new = cs $ symbolVal (Proxy :: Proxy path)

instance HasGenRequest EmptyAPI where
  genRequest _ = (0, error "EmptyAPIs cannot be queried.")

instance HasGenRequest api => HasGenRequest (Summary d :> api) where
  genRequest _ = genRequest (Proxy :: Proxy api)

instance HasGenRequest api => HasGenRequest (Description d :> api) where
  genRequest _ = genRequest (Proxy :: Proxy api)

instance (Arbitrary c, HasGenRequest b, ToHttpApiData c )
    => HasGenRequest (Capture' mods x c :> b) where
    genRequest _ = (oldf, do
      old' <- old
      new' <- toUrlPiece <$> new
      return $ \burl -> let r = old' burl in r { path = cs new' <> path r })
      where
        (oldf, old) = genRequest (Proxy :: Proxy b)
        new = arbitrary :: Gen c

instance (Arbitrary c, HasGenRequest b, ToHttpApiData c )
    => HasGenRequest (CaptureAll x c :> b) where
    genRequest _ = (oldf, do
      old' <- old
      new' <- fmap (cs . toUrlPiece) <$> new
      let new'' = BS.intercalate "/" new'
      return $ \burl -> let r = old' burl in r { path = new'' <> path r })
      where
        (oldf, old) = genRequest (Proxy :: Proxy b)
        new = arbitrary :: Gen [c]

instance (Arbitrary c, KnownSymbol h, HasGenRequest b, ToHttpApiData c)
    => HasGenRequest (Header' mods h c :> b) where
    genRequest _ = (oldf, do
      old' <- old
      new' <- toUrlPiece <$> new  -- TODO: generate lenient or/and optional
      return $ \burl -> let r = old' burl in r {
          requestHeaders = (hdr, cs new') : requestHeaders r })
      where
        (oldf, old) = genRequest (Proxy :: Proxy b)
        hdr = fromString $ symbolVal (Proxy :: Proxy h)
        new = arbitrary :: Gen c

instance (AllMimeRender x c, Arbitrary c, HasGenRequest b)
    => HasGenRequest (ReqBody' mods x c :> b) where
    genRequest _ = (oldf, do
      old' <- old  -- TODO: generate lenient
      new' <- new
      (ct, bd) <- elements $ allMimeRender (Proxy :: Proxy x) new'
      return $ \burl -> let r = old' burl in r {
          requestBody = RequestBodyLBS bd
        , requestHeaders = ("Content-Type", renderHeader ct) : requestHeaders r
        })
      where
        (oldf, old) = genRequest (Proxy :: Proxy b)
        new = arbitrary :: Gen c

instance (KnownSymbol x, Arbitrary c, ToHttpApiData c, HasGenRequest b)
    => HasGenRequest (QueryParam' mods x c :> b) where
    genRequest _ = (oldf, do
      new' <- new  -- TODO: generate lenient or/and optional
      old' <- old
      return $ \burl -> let r = old' burl
                            newExpr = param <> "=" <> cs (toQueryParam new')
                            qs = queryString r in r {
          queryString = if BS.null qs then newExpr else newExpr <> "&" <> qs })
      where
        (oldf, old) = genRequest (Proxy :: Proxy b)
        param = cs $ symbolVal (Proxy :: Proxy x)
        new = arbitrary :: Gen c

instance (KnownSymbol x, Arbitrary c, ToHttpApiData c, HasGenRequest b)
    => HasGenRequest (QueryParams x c :> b) where
    genRequest _ = (oldf, do
      new' <- new
      old' <- old
      return $ \burl -> let r = old' burl in r {
          queryString = queryString r
                     <> if length new' > 0 then fold (toParam <$> new') else ""})
      where
        (oldf, old) = genRequest (Proxy :: Proxy b)
        param = cs $ symbolVal (Proxy :: Proxy x)
        new = arbitrary :: Gen [c]
        toParam c = param <> "[]=" <> cs (toQueryParam c)
        fold = foldr1 (\a b -> a <> "&" <> b)

instance (KnownSymbol x, HasGenRequest b)
    => HasGenRequest (QueryFlag x :> b) where
    genRequest _ = (oldf, do
      old' <- old
      return $ \burl -> let r = old' burl
                            qs = queryString r in r {
          queryString = if BS.null qs then param else param <> "&" <> qs })
      where
        (oldf, old) = genRequest (Proxy :: Proxy b)
        param = cs $ symbolVal (Proxy :: Proxy x)

instance (ReflectMethod method)
    => HasGenRequest (Verb (method :: k) (status :: Nat) (cts :: [*]) a) where
    genRequest _ = (1, return $ \burl -> defaultRequest
       { host = cs $ baseUrlHost burl
       , port = baseUrlPort burl
       , secure = baseUrlScheme burl == Https
       , method = reflectMethod (Proxy :: Proxy method)
       })

instance (ReflectMethod method)
    => HasGenRequest (NoContentVerb (method :: k)) where
    genRequest _ = (1, return $ \burl -> defaultRequest
       { host = cs $ baseUrlHost burl
       , port = baseUrlPort burl
       , secure = baseUrlScheme burl == Https
       , method = reflectMethod (Proxy :: Proxy method)
       })

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

instance (HasGenRequest a) => HasGenRequest (Fragment v :> a) where
    genRequest _ = genRequest (Proxy :: Proxy a)
