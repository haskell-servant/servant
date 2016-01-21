{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Servant.API.WithNamedConfig where

import GHC.TypeLits

-- | 'WithNamedConfig' names a specific tagged configuration to use for the
-- combinators in the API. (See also in @servant-server@,
-- @Servant.Server.Config@.) For example:
--
-- > type UseNamedConfigAPI = WithNamedConfig "myConfig" '[String] (
-- >     ReqBody '[JSON] Int :> Get '[JSON] Int)
--
-- Both the 'ReqBody' and 'Get' combinators will use the 'NamedConfig' with
-- type tag "myConfig" as their configuration.
--
-- 'Config's are only relevant for @servant-server@.
--
-- For more information, see the tutorial.
data WithNamedConfig (name :: Symbol) (subConfig :: [*]) subApi
