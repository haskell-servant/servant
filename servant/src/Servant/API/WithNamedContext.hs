{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

module Servant.API.WithNamedContext where

import           GHC.TypeLits

-- | 'WithNamedContext' names a specific tagged context to use for the
-- combinators in the API. (See also in @servant-server@,
-- @Servant.Server.Context@.) For example:
--
-- > type UseNamedContextAPI = WithNamedContext "myContext" '[String] (
-- >     ReqBody '[JSON] Int :> Get '[JSON] Int)
--
-- Both the 'ReqBody' and 'Get' combinators will use the 'WithNamedContext' with
-- type tag "myContext" as their context.
--
-- 'Context's are only relevant for @servant-server@.
--
-- For more information, see the tutorial.
data WithNamedContext (name :: Symbol) (subContext :: [*]) subApi
