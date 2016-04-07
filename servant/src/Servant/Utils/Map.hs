{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Servant.Utils.Map (mapLeaves) where

import           Servant.API

class Mappable s t a b where
  mapLeaves :: (forall x . a x -> b x) -> s -> t

instance Mappable (a x) (b x) a b where
  mapLeaves f a = f a

instance Mappable s t a b => Mappable (arg -> s) (arg -> t) a b where
  mapLeaves f s = mapLeaves f $ s

instance (Mappable left left' a b, Mappable right right' a b) => Mappable (left :<|> right) (left' :<|> right') a b where
  mapLeaves f (left :<|> right) = mapLeaves f left :<|> mapLeaves f right
