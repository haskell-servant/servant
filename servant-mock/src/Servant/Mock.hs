{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
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
import           Test.QuickCheck.Arbitrary  (Arbitrary (..), vector)
import           Test.QuickCheck.Gen        (Gen, generate)

-- | 'HasMock' defines an interpretation of API types
--   than turns them into random-response-generating
--   request handlers, hence providing an instance for
--   all the combinators of the core /servant/ library.
class HasServer api => HasMock api where
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
  --   getUser :: EitherT ServantErr IO User
  --   getBook :: EitherT ServantErr IO Book
  --   @
  --
  --   So under the hood, 'mock' uses the 'IO' bit to generate
  --   random values of type 'User' and 'Book' every time these
  --   endpoints are requested.
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

    where genBody = pack <$> generate (vector 100 :: Gen [Char])

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


