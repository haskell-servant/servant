{-# LANGUAGE PolyKinds #-}
module Servant.API.Header where

import GHC.TypeLits

-- | Extract the given header's value as a value of type @a@.
--
-- Example:
--
-- > newtype Referer = Referer Text
-- >   deriving (Eq, Show, FromText, ToText)
-- >
-- >            -- GET /view-my-referer
-- > type MyApi = "view-my-referer" :> Header "from" Referer :> Get Referer
data Header sym a
