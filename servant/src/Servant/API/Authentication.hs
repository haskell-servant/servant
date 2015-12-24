{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Authentication
( AuthPolicy (..)
, AuthProtect
, AuthProtected (..)
, BasicAuth (..)
, JWTAuth (..)
, OnMissing (..)
, OnUnauthenticated (..)
, SAuthPolicy (..)
) where


import           Data.ByteString (ByteString)
import           Data.Typeable   (Typeable)
import           GHC.TypeLits    (Symbol)
import           Data.Text       (Text)

-- | we can be either Strict or Lax.
-- Strict: all handlers under 'AuthProtect' take a 'usr' argument.
--         when auth fails, we call user-supplied handlers to respond.
-- Lax: all handlers under 'AuthProtect' take a 'Maybe usr' argument.
--      when auth fails, we call the handlers with 'Nothing'.
data AuthPolicy = Strict | Lax

-- | Singleton types for AuthPolicy
data SAuthPolicy (p :: AuthPolicy) where
    SStrict :: SAuthPolicy 'Strict
    SLax    :: SAuthPolicy 'Lax

-- | the combinator to be used in API types
data AuthProtect authData usr (missingPolicy :: AuthPolicy) missingError (unauthPolicy :: AuthPolicy) unauthError

-- | A GADT indexed by policy strictness that encompasses the ways
-- users will handle the case where authentication data is missing
-- from a request. For example, suppose we have a Basic-Auth-protected
-- resource and no appropriate headers are present in the request. Then
-- the specified 'OnMissing' will be used. In the case it is Lax, then
-- the servant framework will issue a generalized response. In the case
-- it is Strict, then api authors can specify how to handle the response.
-- About the type parameters:
-- m: the monad errors are retrned in. For now just IO.
-- responseError: an error response type. For now just ServantErr.
-- policy: the policy to handle OnMissing.
-- errorIndex: an ADT representing possible errors encountered while extracting
-- authentication data from a request.
data OnMissing m responseError (policy :: AuthPolicy) errorIndex where
    LaxMissing :: OnMissing m responseError 'Lax errorIndex
    StrictMissing :: (errorIndex -> m responseError) -> OnMissing m responseError 'Strict errorIndex

-- | A GADT indexed by policy strictness that encompasses the ways
-- users will handle the case where the authentication data provided is
-- rejected. I.e. a username and password do not match in the database.
-- About the type parameters:
-- m: the monad errors are returned in. For now just IO.
-- responseError: an error response type. For now just ServantErr.
-- authData: the authentication data extracted from the request
-- errorIndex: an index of error to give a user-provided, semantic meaning to the authentication failure.
-- policy: the policy to handle OnUnauthenticated actions.
data OnUnauthenticated m responseError (policy :: AuthPolicy) errorIndex authData where
    LaxUnauthenticated :: OnUnauthenticated m e 'Lax errorIndex authData
    StrictUnauthenticated :: (errorIndex -> authData -> m responseError)
                          -> OnUnauthenticated m responseError 'Strict errorIndex authData

-- | A GADT representing the data and functions required to protect a reasource for authentication.
-- For an authenticated resource, we need to handle the scenario where authentication data is missing
-- and where authentication data is present but not valid (e.g. uesrname + password not valid).
-- m: the monad errors are retrned in. For now just IO.
-- rError: an error response type. For now just ServantErr.
-- mPolicy: Missing Auth: the policy to handle missing authentication data actions.
-- mError: Missing Auth: ADT error index for possible missing auth failures
-- uPolicy: Unauthenticated: the policy to handle rejected authentication attempts.
-- uError: Unauthenticated: ADT error index for possible unauthentication failures
-- authData: the type of authData present in a request (e.g. JWT token)
-- usr: a data type extracted from the authenticated data. This data is likely fetched from a database.
-- subserver: the rest of the servant API.
data AuthProtected m rError (mPolicy :: AuthPolicy) mError (uPolicy :: AuthPolicy) uError authData usr subserver =
    AuthProtected { onMissing :: OnMissing m rError mPolicy mError
                  , onUnauthenticated :: OnUnauthenticated m rError uPolicy uError authData
                  , checkAuth :: authData -> m (Either uError usr)
                  , subserver :: subserver
                  }

-- | Basic Authentication with respect to a specified @realm@ and a @lookup@
-- type to encapsulate authentication logic.
data BasicAuth (realm :: Symbol) = BasicAuth { baUser :: ByteString
                                             , baPass :: ByteString
                                             } deriving (Eq, Show, Typeable)

newtype JWTAuth = JWTAuth { unJWTAuth :: Text }
