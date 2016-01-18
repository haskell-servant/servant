{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Servant.API.WithNamedConfig where

import GHC.TypeLits

-- | 'WithNamedConfig' names a specific tagged configuration to use for the
-- combinators in the API. For example:
--
-- > type UseNamedConfigAPI1 = WithNamedConfig "myConfig" '[String] (
-- >     ReqBody '[JSON] Int :> Get '[JSON] Int)
--
-- Both the 'ReqBody' and 'Get' combinators will use the 'NamedConfig' with
-- type tag "myConfig" as their configuration. In constrast, in (notice
-- parentesizing):
--
-- > type UseNamedConfigAPI2 = WithNamedConfig "myConfig" '[String] (
-- >     ReqBody '[JSON] Int) :> Get '[JSON] Int
--
-- Only the 'ReqBody' combinator will use this configuration, and 'Get' will
-- maintain the default configuration.
--
-- For more information, see the tutorial.
data WithNamedConfig (name :: Symbol) (subConfig :: [*]) subApi
