{-# OPTIONS_HADDOCK not-home    #-}
-- | Define API combinator that modify the behaviour of the routing environment.
module Servant.API.Environment (WithRoutingHeader) where

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

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Foo
-- >>> instance ToJSON Foo where { toJSON = undefined }
