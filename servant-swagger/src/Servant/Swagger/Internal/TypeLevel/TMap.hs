{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Servant.Swagger.Internal.TypeLevel.TMap where

import           Data.Proxy
import           GHC.Exts   (Constraint)

-- $setup
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XGADTs
-- >>> :set -XRankNTypes
-- >>> :set -XScopedTypeVariables
-- >>> import GHC.TypeLits
-- >>> import Data.List

-- | Map a list of constrained types to a list of values.
--
-- >>> tmap (Proxy :: Proxy KnownSymbol) symbolVal (Proxy :: Proxy ["hello", "world"])
-- ["hello","world"]
class TMap (q :: k -> Constraint) (xs :: [k]) where
  tmap :: p q -> (forall x p'. q x => p' x -> a) -> p'' xs -> [a]

instance TMap q '[] where
  tmap _ _ _ = []

instance (q x, TMap q xs) => TMap q (x ': xs) where
  tmap q f _ = f (Proxy :: Proxy x) : tmap q f (Proxy :: Proxy xs)

