module Servant.API.HttpVersion
  ( -- $httpversion
    HttpVersion(..)
  ) where

import           Network.HTTP.Types (HttpVersion (..))

-- $httpversion
--
-- | You can directly use the 'HttpVersion' type from @Network.HTTP.Types@
--   if your request handlers need it to compute a response. This would
--   make the request handlers take an argument of type 'HttpVersion'.
--
-- Example:
--
-- >>> type API = HttpVersion :> Get '[JSON] String

-- $setup
-- >>> import Servant.API
