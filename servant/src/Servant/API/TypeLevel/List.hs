module Servant.API.TypeLevel.List 
    (type (.++)
    ) where

import Data.Kind

type family (.++) (l1 :: [Type]) (l2 :: [Type]) where
  '[] .++ a = a
  (a ': as) .++ b = a ': (as .++ b)
