{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Servant.Mock ( HasMock(..) ) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative         ((<$>))
#endif
import Control.Monad.IO.Class
import Data.ByteString.Lazy.Char8 (pack)
import Data.Proxy
import GHC.TypeLits
import Network.HTTP.Types.Status
import Network.Wai
import Servant
import Servant.API.ContentTypes
import Test.QuickCheck.Arbitrary (Arbitrary(..), vector)
import Test.QuickCheck.Gen (Gen, generate)

class HasServer api => HasMock api where
  mock :: Proxy api -> Server api

instance (HasMock a, HasMock b) => HasMock (a :<|> b) where
  mock _ = mock (Proxy :: Proxy a) :<|> mock (Proxy :: Proxy b)

instance (KnownSymbol path, HasMock rest) => HasMock (path :> rest) where
  mock _ = mock (Proxy :: Proxy rest)

instance (KnownSymbol s, FromText a, HasMock rest) => HasMock (Capture s a :> rest) where
  mock _ = \_ -> mock (Proxy :: Proxy rest)

instance (AllCTUnrender ctypes a, HasMock rest) => HasMock (ReqBody ctypes a :> rest) where
  mock _ = \_ -> mock (Proxy :: Proxy rest)

instance HasMock rest => HasMock (RemoteHost :> rest) where
  mock _ = \_ -> mock (Proxy :: Proxy rest)

instance HasMock rest => HasMock (IsSecure :> rest) where
  mock _ = \_ -> mock (Proxy :: Proxy rest)

instance HasMock rest => HasMock (Vault :> rest) where
  mock _ = \_ -> mock (Proxy :: Proxy rest)

instance HasMock rest => HasMock (HttpVersion :> rest) where
  mock _ = \_ -> mock (Proxy :: Proxy rest)

instance (KnownSymbol s, FromText a, HasMock rest)
      => HasMock (QueryParam s a :> rest) where
  mock _ = \_ -> mock (Proxy :: Proxy rest)

instance (KnownSymbol s, FromText a, HasMock rest)
      => HasMock (QueryParams s a :> rest) where
  mock _ = \_ -> mock (Proxy :: Proxy rest)

instance (KnownSymbol s, HasMock rest) => HasMock (QueryFlag s :> rest) where
  mock _ = \_ -> mock (Proxy :: Proxy rest)

instance (KnownSymbol s, FromText a, HasMock rest)
      => HasMock (MatrixParam s a :> rest) where
  mock _ = \_ -> mock (Proxy :: Proxy rest)

instance (KnownSymbol s, FromText a, HasMock rest)
      => HasMock (MatrixParams s a :> rest) where
  mock _ = \_ -> mock (Proxy :: Proxy rest)

instance (KnownSymbol s, HasMock rest) => HasMock (MatrixFlag s :> rest) where
  mock _ = \_ -> mock (Proxy :: Proxy rest)

instance (KnownSymbol h, FromText a, HasMock rest) => HasMock (Header h a :> rest) where
  mock _ = \_ -> mock (Proxy :: Proxy rest)

instance (Arbitrary a, AllCTRender ctypes a) => HasMock (Delete ctypes a) where
  mock _ = mockArbitrary

instance (Arbitrary a, AllCTRender ctypes a) => HasMock (Get ctypes a) where
  mock _ = mockArbitrary

instance (Arbitrary a, AllCTRender ctypes a) => HasMock (Patch ctypes a) where
  mock _ = mockArbitrary

instance (Arbitrary a, AllCTRender ctypes a) => HasMock (Post ctypes a) where
  mock _ = mockArbitrary

instance (Arbitrary a, AllCTRender ctypes a) => HasMock (Put ctypes a) where
  mock _ = mockArbitrary

instance HasMock Raw where
  mock _ = \req respond -> do
    bdy <- genBody
    respond $ responseLBS status200 [] bdy

    where genBody = fmap pack $ generate (vector 100 :: Gen [Char])

mockArbitrary :: (MonadIO m, Arbitrary a) => m a
mockArbitrary = liftIO (generate arbitrary)

-- utility instance
instance (Arbitrary (HList ls), Arbitrary a)
      => Arbitrary (Headers ls a) where
  arbitrary = Headers <$> arbitrary <*> arbitrary

instance Arbitrary (HList '[]) where
  arbitrary = pure HNil

instance (Arbitrary a, Arbitrary (HList hs))
      => Arbitrary (HList (Header h a ': hs)) where
  arbitrary = HCons <$> fmap Header arbitrary <*> arbitrary


