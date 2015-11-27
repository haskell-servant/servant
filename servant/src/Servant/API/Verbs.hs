{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE PolyKinds          #-}
module Servant.API.Verbs where

import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)
import           GHC.TypeLits              (Nat)
import           Network.HTTP.Types.Method (Method, StdMethod (..),
                                            methodDelete, methodGet, methodHead,
                                            methodPatch, methodPost, methodPut)

-- | @Verb@ is a general type for representing HTTP verbs/methods. For
-- convenience, type synonyms for each verb with a 200 response code are
-- provided, but you are free to define your own:
--
-- >>> type Post204 contentTypes a = Verb 'POST 204 contentTypes a
data Verb (method :: k1) (statusCode :: Nat) (contentTypes :: [*]) a
  deriving (Typeable, Generic)

-- 'GET' with 200 status code.
type Get    contentTypes a = Verb 'GET    200 contentTypes a

-- 'POST' with 200 status code.
type Post   contentTypes a = Verb 'POST   200 contentTypes a

-- 'PUT' with 200 status code.
type Put    contentTypes a = Verb 'PUT    200 contentTypes a

-- 'DELETE' with 200 status code.
type Delete contentTypes a = Verb 'DELETE 200 contentTypes a

-- 'PATCH' with 200 status code.
type Patch  contentTypes a = Verb 'PATCH  200 contentTypes a

-- 'HEAD' with 200 status code.
type Head   contentTypes a = Verb 'HEAD   200 contentTypes a

class ReflectMethod a where
    reflectMethod :: proxy a -> Method

instance ReflectMethod 'GET where
    reflectMethod _ = methodGet

instance ReflectMethod 'POST where
    reflectMethod _ = methodPost

instance ReflectMethod 'PUT where
    reflectMethod _ = methodPut

instance ReflectMethod 'DELETE where
    reflectMethod _ = methodDelete

instance ReflectMethod 'PATCH where
    reflectMethod _ = methodPatch

instance ReflectMethod 'HEAD where
    reflectMethod _ = methodHead
