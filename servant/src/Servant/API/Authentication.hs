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

-- | the combinator to be used in API types
data AuthProtect authdata usr (policy :: AuthPolicy)

-- | A GADT indexed by policy strictness that encompasses the ways
-- users will handle the case where authentication data is missing
-- from a request. For example, suppose we have a Basic-Auth-protected
-- resource and no appropriate headers are present in the request. Then
-- the specified 'OnMissing' will be used. In the case it is Lax, then
-- the servant framework will issue a generalized response. In the case
-- it is Strict, then api authors can specify how to handle the response.
-- About the type parameters:
-- m: the monad errors are retrned in. For now just IO.
-- e: an error type. For now just ServantErr.
-- policy: the policy to handle OnMissing.
data OnMissing m e (policy :: AuthPolicy) where
    LaxMissing :: OnMissing m e 'Lax
    StrictMissing :: m e -> OnMissing m e 'Strict

-- | A GADT indexed by policy strictness that encompasses the ways
-- users will handle the case where the authentication data provided is
-- rejected. I.e. a username and password do not match in the database.
-- About the type parameters:
-- m: the monad errors are returned in. For now just IO.
-- e: an error type. For now just ServantErr.
-- authData: the authentication data extracted from the request
-- errorIndex: an index of error to give a user-provided, semantic meaning to the authentication failure.
-- policy: the policy to handle OnUnauthenticated actions.
data OnUnauthenticated m e (policy :: AuthPolicy) errorIndex authData where
    LaxUnauthenticated :: OnUnauthenticated m e 'Lax errorIndex authData
    StrictUnauthenticated :: (errorIndex -> authData -> m e) -> OnUnauthenticated m e 'Strict errorIndex authData

-- | A GADT representing the data and functions required to protect a reasource for authentication.
-- For an authenticated resource, we need to handle the scenario where authentication data is missing
-- and where authentication data is present but not valid (e.g. uesrname + password not valid).
-- m: the monad errors are retrned in. For now just IO.
-- e: an error type. For now just ServantErr.
-- missingPolicy: the policy to handle missing authentication data actions.
-- unauthPolicy: the policy to handle rejected authentication attempts.
-- authData: the type of authData present in a request (e.g. JWT token)
-- errorIndex: an index of error to give a user-provided, semantic meaning to the authentication failure.
-- usr: a data type extracted from the authenticated data. This data is likely fetched from a database.
-- subserver: the rest of the servant API.
data AuthProtected m e (missingPolicy :: AuthPolicy) (unauthPolicy :: AuthPolicy) errorIndex authData usr subserver =
    AuthProtected { onMissing :: OnMissing m e missingPolicy
                  , onUnathenticated :: OnUnauthenticated m e unauthPolicy errorIndex authData
                  , checkAuth :: authData -> m (Either errorIndex usr)
                  , subserver :: subserver
                  }

-- | Basic Authentication with respect to a specified @realm@ and a @lookup@
-- type to encapsulate authentication logic.
data BasicAuth (realm :: Symbol) = BasicAuth { baUser :: ByteString
                                             , baPass :: ByteString
                                             } deriving (Eq, Show, Typeable)

newtype JWTAuth = JWTAuth { unJWTAuth :: Text }
