module Servant.QuickCheck.Internal.ErrorTypes where

import Text.PrettyPrint

data Request = Request
  { requestBody    :: String
  , requestHeaders :: [String]
  , requestUrl     :: String
  } deriving (Eq, Show, Read, Generic)

prettyReq :: Doc
prettyReq r =
  text "Request:" $ nest 5 $
     text "URL:"      <+> text (nest 5 $ requestUrl r)
  $$ text "Headers:"  <+>
  $$ text "Body:"     <+> text (nest 5 $ requestBody r)

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
  deriving (Eq, Show, Read, Generic)

instance Show Failure where
  show (PredicateFailure pred req resp)
    = "Predicate failed for " <> pred <> "
