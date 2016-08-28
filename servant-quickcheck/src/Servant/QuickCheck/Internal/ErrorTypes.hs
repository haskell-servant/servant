module Servant.QuickCheck.Internal.ErrorTypes where

import Text.PrettyPrint
import Prelude.Compat
import Data.String (IsString(fromString))
import GHC.Generics (Generic)

data Request = Request
  { requestBody    :: String
  , requestHeaders :: [String]
  , requestUrl     :: String
  } deriving (Eq, Show, Read, Generic)

prettyReq :: Request -> Doc
prettyReq r =
  text "Request:" $$ (nest 5 $
     text "URL:"      <+> (nest 5 $ text $ requestUrl r)
  $$ text "Headers:"  <+> (nest 5 $ hsep $ text <$> requestHeaders r)
  $$ text "Body:"     <+> (nest 5 $ text $ requestBody r))

instance IsString Request where
  fromString url = Request "" [] url

data Response = Response
  { responseBody    :: String
  , responseHeaders :: [String]
  } deriving (Eq, Show, Read, Generic)

instance IsString Response where
  fromString body = Response body []

-- The error that occurred.
data Failure
  = PredicateFailure String Request Response
  | ServerEqualityFailure Request Response Response
  deriving (Eq, Read, Generic)

instance Show Failure where
  show (PredicateFailure pred req resp)
    = "Predicate failed for " ++ pred
