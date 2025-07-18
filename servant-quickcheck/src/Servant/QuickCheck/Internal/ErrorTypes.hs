{-# LANGUAGE CPP #-}

module Servant.QuickCheck.Internal.ErrorTypes where

import Control.Exception (Exception (..))
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as T
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as C
import Network.HTTP.Types (Header, statusCode)
import Prelude.Compat hiding ((<>))
import Text.PrettyPrint

data PredicateFailure
  = PredicateFailure T.Text (Maybe C.Request) (C.Response LBS.ByteString)
  deriving (Generic, Typeable)

instance Exception ServerEqualityFailure

instance Show PredicateFailure where
  show = render . prettyPredicateFailure

data ServerEqualityFailure
  = ServerEqualityFailure C.Request (C.Response LBS.ByteString) (C.Response LBS.ByteString)
  deriving (Generic, Typeable)

instance Show ServerEqualityFailure where
  show = render . prettyServerEqualityFailure

instance Exception PredicateFailure

-- * Pretty printing

prettyHeaders :: [Header] -> Doc
prettyHeaders hdrs = vcat $ prettyHdr <$> hdrs
  where
    prettyHdr (hn, h) = text (show hn) <> colon <+> text (show h)

prettyReq :: C.Request -> Doc
prettyReq r =
  text "Request:"
    $$ nest
      5
      ( text "Method:" <+> nest 5 (text . show $ C.method r)
          $$ text "Path:" <+> nest 5 (text . BS8.unpack $ C.path r)
          $$ text "Headers:" <+> nest 5 (prettyHeaders $ C.requestHeaders r)
          $$ text "Body:" <+> nest 5 (text . getReqBody $ C.requestBody r)
      )
  where
    getReqBody :: C.RequestBody -> String
    getReqBody (C.RequestBodyLBS lbs) = BSL8.unpack lbs
    getReqBody (C.RequestBodyBS bs) = BS8.unpack bs
    getReqBody _ = error "expected bytestring body"

prettyResp :: C.Response LBS.ByteString -> Doc
prettyResp r =
  text "Response:"
    $$ nest
      5
      ( text "Status code:" <+> nest 5 (text . show . statusCode $ C.responseStatus r)
          $$ text "Headers:"
          $$ nest 10 (prettyHeaders $ C.responseHeaders r)
          $$ text "Body:" <+> nest 5 (text . BSL8.unpack $ C.responseBody r)
      )

prettyServerEqualityFailure :: ServerEqualityFailure -> Doc
prettyServerEqualityFailure (ServerEqualityFailure req resp1 resp2) =
  text "Server equality failed"
    $$ nest
      5
      ( prettyReq req
          $$ prettyResp resp1
          $$ prettyResp resp2
      )

prettyPredicateFailure :: PredicateFailure -> Doc
prettyPredicateFailure (PredicateFailure predicate req resp) =
  text "Predicate failed"
    $$ nest
      5
      ( text "Predicate:" <+> text (T.unpack predicate)
          $$ r
          $$ prettyResp resp
      )
  where
    r = case req of
      Nothing -> text ""
      Just v -> prettyReq v
