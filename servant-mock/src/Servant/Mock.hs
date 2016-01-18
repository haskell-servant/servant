{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#include "overlapping-compat.h"

-- |
-- Module     : Servant.Mock
-- Copyright  : 2015 Alp Mestanogullari
-- License    : BSD3
--
-- Maintainer  : Alp Mestanogullari <alpmestan@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
-- Automatically derive a mock webserver that implements some API type,
-- just from the said API type's definition.
--
-- Using this module couldn't be simpler. Given some API type, like:
--
-- > type API = "user" :> Get '[JSON] User
--
-- that describes your web application, all you have to do is define
-- a 'Proxy' to it:
--
-- > myAPI :: Proxy API
-- > myAPI = Proxy
--
-- and call 'mock', which has the following type:
--
-- @
-- 'mock' :: 'HasMock' api => 'Proxy' api -> 'Server' api
-- @
--
-- What this says is, given some API type @api@ that it knows it can
-- "mock", 'mock' hands you an implementation of the API type. It does so
-- by having each request handler generate a random value of the
-- appropriate type (@User@ in our case). All you need for this to work is
-- to provide 'Arbitrary' instances for the data types returned as response
-- bodies, hence appearing next to 'Delete', 'Get', 'Patch', 'Post' and 'Put'.
--
-- To put this all to work and run the mock server, just call 'serve' on the
-- result of 'mock' to get an 'Application' that you can then run with warp.
--
-- @
-- main :: IO ()
-- main = Network.Wai.Handler.Warp.run 8080 $
--   'serve' myAPI ('mock' myAPI)
-- @
module Servant.Mock ( HasMock(..) ) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import           Control.Monad.IO.Class
import           Data.ByteString.Lazy.Char8 (pack)
import           Data.Proxy
import           GHC.TypeLits
import           Network.HTTP.Types.Status
import           Network.Wai
import           Servant
import           Servant.API.ContentTypes
import           Servant.Server.Internal.Config
import           Test.QuickCheck.Arbitrary  (Arbitrary (..), vector)
import           Test.QuickCheck.Gen        (Gen, generate)

-- | 'HasMock' defines an interpretation of API types
--   than turns them into random-response-generating
--   request handlers, hence providing an instance for
--   all the combinators of the core /servant/ library.
class HasServer api config => HasMock api config where
  -- | Calling this method creates request handlers of
  --   the right type to implement the API described by
  --   @api@ that just generate random response values of
  --   the right type. E.g:
  --
  --   @
  --   type API = "user" :> Get '[JSON] User
  --         :<|> "book" :> Get '[JSON] Book
  --
  --   api :: Proxy API
  --   api = Proxy
  --
  --   -- let's say we will start with the frontend,
  --   -- and hence need a placeholder server
  --   server :: Server API
  --   server = mock api
  --   @
  --
  --   What happens here is that @'Server' API@
  --   actually "means" 2 request handlers, of the following types:
  --
  --   @
  --   getUser :: ExceptT ServantErr IO User
  --   getBook :: ExceptT ServantErr IO Book
  --   @
  --
  --   So under the hood, 'mock' uses the 'IO' bit to generate
  --   random values of type 'User' and 'Book' every time these
  --   endpoints are requested.
  mock :: Proxy api -> Proxy config -> Server api

instance (HasMock a config, HasMock b config) => HasMock (a :<|> b) config where
  mock _ config = mock (Proxy :: Proxy a) config :<|> mock (Proxy :: Proxy b) config

instance (KnownSymbol path, HasMock rest config) => HasMock (path :> rest) config where
  mock _ = mock (Proxy :: Proxy rest)

instance (KnownSymbol s, FromHttpApiData a, HasMock rest config) => HasMock (Capture s a :> rest) config where
  mock _ config = \_ -> mock (Proxy :: Proxy rest) config

instance (AllCTUnrender ctypes a, HasMock rest config) => HasMock (ReqBody ctypes a :> rest) config where
  mock _ config = \_ -> mock (Proxy :: Proxy rest) config

instance HasMock rest config => HasMock (RemoteHost :> rest) config where
  mock _ config = \_ -> mock (Proxy :: Proxy rest) config

instance HasMock rest config => HasMock (IsSecure :> rest) config where
  mock _ config = \_ -> mock (Proxy :: Proxy rest) config

instance HasMock rest config => HasMock (Vault :> rest) config where
  mock _ config = \_ -> mock (Proxy :: Proxy rest) config

instance HasMock rest config => HasMock (HttpVersion :> rest) config where
  mock _ config = \_ -> mock (Proxy :: Proxy rest) config

instance (KnownSymbol s, FromHttpApiData a, HasMock rest config)
      => HasMock (QueryParam s a :> rest) config where
  mock _ config = \_ -> mock (Proxy :: Proxy rest) config

instance (KnownSymbol s, FromHttpApiData a, HasMock rest config)
      => HasMock (QueryParams s a :> rest) config where
  mock _ config = \_ -> mock (Proxy :: Proxy rest) config

instance (KnownSymbol s, HasMock rest config) => HasMock (QueryFlag s :> rest) config where
  mock _ config = \_ -> mock (Proxy :: Proxy rest) config

instance (KnownSymbol h, FromHttpApiData a, HasMock rest config) => HasMock (Header h a :> rest) config where
  mock _ config = \_ -> mock (Proxy :: Proxy rest) config

instance (Arbitrary a, KnownNat status, ReflectMethod method, AllCTRender ctypes a)
    => HasMock (Verb method status ctypes a) config where
  mock _ _ = mockArbitrary

instance OVERLAPPING_
    (GetHeaders (Headers headerTypes a), Arbitrary (HList headerTypes),
     Arbitrary a, KnownNat status, ReflectMethod method, AllCTRender ctypes a)
    => HasMock (Verb method status ctypes (Headers headerTypes a)) config where
  mock _ _ = mockArbitrary

instance HasMock Raw config where
  mock _ _ = \_req respond -> do
    bdy <- genBody
    respond $ responseLBS status200 [] bdy

    where genBody = pack <$> generate (vector 100 :: Gen [Char])

instance (HasConfigEntry config (NamedConfig name subConfig), HasMock rest subConfig) =>
  HasMock (WithNamedConfig name subConfig rest) config where

  mock _ _ = mock (Proxy :: Proxy rest) (Proxy :: Proxy subConfig)

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
