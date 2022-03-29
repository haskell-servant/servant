{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}          
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Servant.Server.Internal.Redirect where

import           Data.SOP.Constraint
                 (All)
import           GHC.TypeLits
                 (Nat, ErrorMessage(..), TypeError)
import           Servant.API
                 ((:>), (:<|>), Raw, Statuses, Stream, UVerb, Verb)
import           Servant.API.Status
                 (HasStatusClass, KnownStatusClass)

type family (as :: [k]) ++ (bs :: [k]) :: [k] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

-- | A type class to gather all statically declared HTTP status codes of an api
class HasApiStatuses a where
  type ApiStatuses a :: [Nat]

instance HasApiStatuses (Verb method status ctypes a) where
  type ApiStatuses (Verb _ status _ _) = '[status]

instance HasApiStatuses (Stream method status framing ctypes a) where
  type ApiStatuses (Stream _ status _ _ _) = '[status]

instance HasApiStatuses (UVerb method ctypes as) where
  type ApiStatuses (UVerb _ _ as) = Statuses as

instance HasApiStatuses Raw where
  type ApiStatuses Raw = TypeError ('Text "cannot observe the HTTP statuses of a Raw API")

instance (HasApiStatuses api) => HasApiStatuses (api' :> api) where
  type ApiStatuses (_ :> api) = ApiStatuses api

instance (HasApiStatuses api1, HasApiStatuses api2) => HasApiStatuses (api1 :<|> api2) where
  type ApiStatuses (api1 :<|> api2) = (ApiStatuses api1) ++ (ApiStatuses api2)

-- | A type class to check that all statically declared HTTP status codes of an api
-- belong to the same status class, as defined by @KnownStatusClass@.
class AllStatusesInClass c api

instance ( HasApiStatuses api
         , KnownStatusClass c
         , All (HasStatusClass c) (ApiStatuses api)
         ) => AllStatusesInClass c api
