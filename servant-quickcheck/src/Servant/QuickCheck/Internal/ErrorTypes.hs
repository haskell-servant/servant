module Servant.QuickCheck.Internal.ErrorTypes where

import           Control.Exception       (Exception (..))
import qualified Data.ByteString.Lazy    as LBS
import           Data.String.Conversions (cs)
import qualified Data.Text               as T
import           GHC.Generics            (Generic)
import qualified Network.HTTP.Client     as C
import           Network.HTTP.Types      (Header, statusCode)
import           Prelude.Compat
import           Text.PrettyPrint

data PredicateFailure
  = PredicateFailure T.Text (Maybe C.Request) (C.Response LBS.ByteString)
  deriving (Generic)

instance Exception ServerEqualityFailure where

instance Show PredicateFailure where
  show = render . prettyPredicateFailure


data ServerEqualityFailure
  = ServerEqualityFailure C.Request (C.Response LBS.ByteString) (C.Response LBS.ByteString)
  deriving (Generic)

instance Show ServerEqualityFailure where
  show = render . prettyServerEqualityFailure


instance Exception PredicateFailure where

-- * Pretty printing

prettyHeaders :: [Header] -> Doc
prettyHeaders hdrs = vcat $ prettyHdr <$> hdrs
  where
    prettyHdr (hn, h) = text (show hn) <> colon <+>  text (show h)

prettyReq :: C.Request -> Doc
prettyReq r =
  text "Request:" $$ (nest 5 $
     text "Method:"   <+> (nest 5 $ text . show $ C.method r)
  $$ text "Path:"     <+> (nest 5 $ text . cs $ C.path r)
  $$ text "Headers:"  <+> (nest 5 $ prettyHeaders $ C.requestHeaders r)
  $$ text "Body:"     <+> (nest 5 $ text . getReqBody $ C.requestBody r))
  where
    getReqBody (C.RequestBodyLBS lbs ) = cs lbs
    getReqBody (C.RequestBodyBS bs ) = cs bs
    getReqBody _ = error "expected bytestring body"

prettyResp :: C.Response LBS.ByteString -> Doc
prettyResp r =
  text "Response:" $$ (nest 5 $
     text "Status code:" <+> (nest 5 $ text . show . statusCode $ C.responseStatus r)
  $$ text "Headers:"  $$ (nest 10 $ prettyHeaders $ C.responseHeaders r)
  $$ text "Body:"     <+> (nest 5 $ text . cs $ C.responseBody r))



prettyServerEqualityFailure :: ServerEqualityFailure -> Doc
prettyServerEqualityFailure (ServerEqualityFailure req resp1 resp2) =
  text "Server equality failed" $$ (nest 5 $
     prettyReq req
  $$ prettyResp resp1
  $$ prettyResp resp2)


prettyPredicateFailure :: PredicateFailure -> Doc
prettyPredicateFailure (PredicateFailure predicate req resp) =
  text "Predicate failed" $$ (nest 5 $
     text "Predicate:" <+> (text $ T.unpack predicate)
  $$ r
  $$ prettyResp resp)
  where
    r = case req of
      Nothing -> text ""
      Just v  -> prettyReq v

