{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE KindSignatures     #-}

{-# OPTIONS_HADDOCK not-home    #-}
-- | Define API combinator that modify the behaviour of the routing environment.
module Servant.API.Environment (Redirect, WithRoutingHeader) where

import           GHC.TypeLits
                 (Symbol)

-- | Modify the behaviour of the following sub-API, such that all endpoint of said API
-- return an additional routing header in their response.
-- A routing header is a header that specifies which endpoint the incoming request was
-- routed to. Endpoint are designated by their path, in which @Capture@ combinators are
-- replaced by a capture hint.
-- This header can be used by downstream middlewares to gather information about
-- individual endpoints, since in most cases a routing header uniquely identifies a
-- single endpoint.
--
-- Example:
--
-- >>> type MyApi = WithRoutingHeader :> "by-id" :> Capture "id" Int :> Get '[JSON] Foo
-- >>>     -- GET /by-id/1234 will return a response with the following header:
-- >>>     --   ("Servant-Routed-Path", "/by-id/<id::Int>")
--
-- @since 0.20
--
data WithRoutingHeader

-- | Modify the behaviour of the following sub-API, such that all endpoints of said API
-- return a "Location" header, set to the value of @location@ type variable. An API using
-- the @Redirect@ combinator **does not typecheck** if any of the endpoints below the
-- combinator returns a status code outside the 3xx range, or if it is used to redirect
-- a @Raw@ API (because we cannot guarantee anything about them).
--
-- For instance, the following API doesn't have a @HasServer@ instance:
--
-- >>> type BadApi
-- >>>   =    "old-api" :> Redirect "/new-api" :> Get '[JSON] Foo
-- >>>   :<|> "new-api" :> Get '[JSON] Foo
-- >>>     -- @Get@ is an alias for @Verb 'GET 200@
--
-- Whereas this one does:
--
-- >>> type GoodApi
-- >>>   =    "old-api" :> Redirect "/new-api" :> Verb 'GET 301 '[JSON] Foo
-- >>>   :<|> "new-api" :> Get '[JSON] Foo
-- >>>     -- GET /old-api will return a response with status 301 and the following header:
-- >>>     --   ("Location", "/new-api")
--
-- @since TODO
--
data Redirect (location :: Symbol)

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Foo
-- >>> instance ToJSON Foo where { toJSON = undefined }
