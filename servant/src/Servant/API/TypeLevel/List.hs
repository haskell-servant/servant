module Servant.API.TypeLevel.List
  ( type (.++)
  )
where

import Data.Kind

-- | Append two type-level lists.
--
-- Import it as
--
-- > import Servant.API.TypeLevel.List (type (.++))
type family (.++) (l1 :: [Type]) (l2 :: [Type]) where
  '[] .++ a = a
  (a ': as) .++ b = a ': (as .++ b)
