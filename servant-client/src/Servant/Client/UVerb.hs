{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Servant.Client.UVerb
  ( collapseUResp,
    extractUResp,
    ClientParseError (..),
  )
where

import Control.Arrow ((+++), left)
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as LB
import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.Functor.Identity (Identity (Identity), runIdentity)
import Data.SOP.BasicFunctors ((:.:) (Comp), K (K))
import Data.SOP.Constraint (All, Constraint)
import Data.SOP.NP (NP (..), cpure_NP)
import Data.SOP.NS (NS (S), cmap_NS, collapse_NS)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.Typeable (Proxy (Proxy), Typeable, cast)
import qualified GHC.Generics as GHC
import Network.HTTP.Media ((//), MediaType, matches, parseAccept)
import Network.HTTP.Types (Status)
import Servant (ReflectMethod, reflectMethod)
import Servant.API.ContentTypes (AllMime (allMime), AllMimeUnrender (allMimeUnrender))
import Servant.API.UVerb (HasStatus, Statuses, UVerb, Union, Unique, inject, statusOf)
import Servant.Client (ClientError (..), HasClient (..), Response, responseBody, responseHeaders, responseStatusCode)
import Servant.Client.Core (RunClient (..), requestAccept, requestMethod, runRequest)

-- | convenience function to extract an unknown union element using a type class.
collapseUResp ::
  forall c a as.
  All c as =>
  Proxy (c :: * -> Constraint) ->
  (forall x. c x => x -> a) ->
  Union as ->
  a
collapseUResp proxy render = collapse_NS . cmap_NS proxy (K . render . runIdentity)

-- | convenience function to extract an unknown union element using 'cast'.
extractUResp :: forall a as. (All Typeable as, Typeable a) => Union as -> Maybe a
extractUResp = collapse_NS . cmap_NS (Proxy @Typeable) (K . cast . runIdentity)

data ClientParseError = ClientParseError MediaType String | ClientStatusMismatch | ClientNoMatchingStatus
  deriving (Eq, Show, GHC.Generic)

instance
  ( RunClient m,
    contentTypes ~ (contentType ': contentTypes'), -- TODO: can we to _ instead of contentTypes'?  probably not.
    as ~ (a ': as'),
    AllMime contentTypes,
    ReflectMethod method,
    All (AllMimeUnrender contentTypes) as,
    All HasStatus as,
    Unique (Statuses as)
  ) =>
  HasClient m (UVerb method contentTypes as)
  where
  type Client m (UVerb method contentTypes as) = m (Union as)

  clientWithRoute _ _ request = do
    let accept = Seq.fromList . allMime $ Proxy @contentTypes
        -- TODO(fisx): we want to send an accept header with, say, the first content type
        -- supported by the api, so we don't have to parse all of them, no?  not sure i'm
        -- missing anything here.

        method = reflectMethod $ Proxy @method
    response <- runRequest request {requestMethod = method, requestAccept = accept}
    responseContentType <- checkContentTypeHeader response
    unless (any (matches responseContentType) accept)
      $ throwClientError
      $ UnsupportedContentType responseContentType response

    let status = responseStatusCode response
        body = responseBody response
        res = tryParsers status $ mimeUnrenders (Proxy @contentTypes) body
    case res of
      Left errors -> throwClientError $ DecodeFailure (T.pack (show errors)) response
      Right x -> return x

  hoistClientMonad _ _ nt s = nt s

-- | Copied from "Servant.Client.Core.HasClient".
checkContentTypeHeader :: RunClient m => Response -> m MediaType
checkContentTypeHeader response =
  case lookup "Content-Type" $ toList $ responseHeaders response of
    Nothing -> return $ "application" // "octet-stream"
    Just t -> case parseAccept t of
      Nothing -> throwClientError $ InvalidContentTypeHeader response
      Just t' -> return t'

-- | Given a list of parsers of 'mkres', returns the first one that succeeds and all the
-- failures it encountered along the way
-- TODO; better name, rewrite haddocs.
tryParsers :: All HasStatus as => Status -> NP ([] :.: Either (MediaType, String)) as -> Either [ClientParseError] (Union as)
tryParsers _ Nil = Left [ClientNoMatchingStatus]
tryParsers status (Comp x :* xs)
  | status == statusOf (Comp x) =
    case partitionEithers x of
      (err', []) -> (map (uncurry ClientParseError) err' ++) +++ S $ tryParsers status xs
      (_, (res : _)) -> Right . inject . Identity $ res
  | otherwise = -- no reason to parse in the first place. This ain't the one we're looking for
    (ClientStatusMismatch :) +++ S $ tryParsers status xs

-- | Given a list of types, parses the given response body as each type
mimeUnrenders ::
  forall contentTypes as.
  All (AllMimeUnrender contentTypes) as =>
  Proxy contentTypes ->
  LB.ByteString ->
  NP ([] :.: Either (MediaType, String)) as
mimeUnrenders ctp body = cpure_NP (Proxy @(AllMimeUnrender contentTypes)) (Comp . map (\(mediaType, parser) -> left ((,) mediaType) (parser body)) . allMimeUnrender $ ctp)
