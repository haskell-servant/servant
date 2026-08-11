{-# LANGUAGE DataKinds #-}

-- | Typed support for the @Accept-Query@ response field defined by
-- <https://www.rfc-editor.org/rfc/rfc10008.html#section-3 RFC 10008, Section 3>.
--
-- This module implements only the Structured Fields subset used by
-- @Accept-Query@. It is not a general RFC 9651 parser.
module Servant.API.AcceptQuery
  ( AcceptQuery
  , AcceptQueryMediaRange
  , AcceptQueryHeader
  , mkAcceptQuery
  , acceptQueryMediaRanges
  , mkAcceptQueryMediaRange
  , acceptQueryType
  , acceptQuerySubtype
  , acceptQueryParameters
  , hAcceptQuery
  )
where

import Control.Applicative (many, optional, (<|>))
import Control.Monad (foldM, unless, when)
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as A
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word (Word8)
import Network.HTTP.Types.Header (hAcceptQuery)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

import Servant.API.Header (Header)

-- | A non-empty, normalized set of media ranges accepted for QUERY request
-- content. Media-range ordering is insignificant under RFC 10008, so values
-- are kept in canonical order.
newtype AcceptQuery = MkAcceptQuery (NonEmpty AcceptQueryMediaRange)
  deriving stock (Eq, Ord, Show)

-- | A QUERY media range and its Structured Fields parameters.
--
-- Construct values with 'mkAcceptQueryMediaRange'. Media types, subtypes, and
-- parameter names are normalized to lowercase; parameter values retain their
-- case because their semantics can be case-sensitive.
data AcceptQueryMediaRange = MkAcceptQueryMediaRange
  { mediaRangeType :: !ByteString
  , mediaRangeSubtype :: !ByteString
  , mediaRangeParameters :: !(Map ByteString ByteString)
  }
  deriving stock (Eq, Ord, Show)

-- | A response header describing the media types accepted as QUERY request
-- content.
type AcceptQueryHeader = Header "Accept-Query" AcceptQuery

-- | Construct an 'AcceptQuery'. The non-empty input prevents constructing an
-- empty field value, and media ranges are sorted because their order is
-- semantically insignificant.
mkAcceptQuery :: NonEmpty AcceptQueryMediaRange -> AcceptQuery
mkAcceptQuery = MkAcceptQuery . NonEmpty.sort

-- | Access the accepted media ranges in canonical order.
acceptQueryMediaRanges :: AcceptQuery -> NonEmpty AcceptQueryMediaRange
acceptQueryMediaRanges (MkAcceptQuery ranges) = ranges

-- | Construct a valid QUERY media range.
--
-- The only accepted wildcard forms are @*/*@ and @type/*@. Parameter names
-- must be representable as RFC 9651 parameter keys, after case
-- normalization, and values must be printable ASCII so they can be rendered
-- as a Structured Fields Token or String.
mkAcceptQueryMediaRange
  :: ByteString
  -- ^ Media type or @*@.
  -> ByteString
  -- ^ Media subtype or @*@.
  -> Map ByteString ByteString
  -- ^ Media-type parameters.
  -> Either Text AcceptQueryMediaRange
mkAcceptQueryMediaRange type_ subtype parameters = do
  validateMediaRange type_ subtype
  normalizedParameters <- foldM insertParameter Map.empty (Map.toList parameters)
  pure
    MkAcceptQueryMediaRange
      { mediaRangeType = asciiLower type_
      , mediaRangeSubtype = asciiLower subtype
      , mediaRangeParameters = normalizedParameters
      }
  where
    insertParameter normalized (name, value) = do
      let normalizedName = asciiLower name
      unless (validParameterKey normalizedName) $ Left "Invalid Accept-Query parameter name"
      unless (BS.all isPrintableAscii value) $ Left "Invalid Accept-Query parameter value"
      when (Map.member normalizedName normalized) $ Left "Duplicate Accept-Query parameter name"
      pure (Map.insert normalizedName value normalized)

-- | Access the normalized media type.
acceptQueryType :: AcceptQueryMediaRange -> ByteString
acceptQueryType = mediaRangeType

-- | Access the normalized media subtype.
acceptQuerySubtype :: AcceptQueryMediaRange -> ByteString
acceptQuerySubtype = mediaRangeSubtype

-- | Access the normalized parameter map.
acceptQueryParameters :: AcceptQueryMediaRange -> Map ByteString ByteString
acceptQueryParameters = mediaRangeParameters

instance ToHttpApiData AcceptQuery where
  toUrlPiece = Text.decodeUtf8 . toHeader
  toHeader = renderAcceptQuery

instance FromHttpApiData AcceptQuery where
  parseUrlPiece = parseHeader . Text.encodeUtf8
  parseHeader = first Text.pack . A.parseOnly acceptQueryParser

renderAcceptQuery :: AcceptQuery -> ByteString
renderAcceptQuery (MkAcceptQuery ranges) =
  LBS.toStrict . Builder.toLazyByteString $
    foldMapWithSeparator (Builder.byteString ", ") renderMediaRange (NonEmpty.toList ranges)

renderMediaRange :: AcceptQueryMediaRange -> Builder.Builder
renderMediaRange (MkAcceptQueryMediaRange type_ subtype parameters) =
  renderBareItem (type_ <> "/" <> subtype)
    <> foldMap renderParameter (Map.toAscList parameters)
  where
    renderParameter (name, value) =
      Builder.word8 semicolon
        <> Builder.byteString name
        <> Builder.word8 equals
        <> renderBareItem value

renderBareItem :: ByteString -> Builder.Builder
renderBareItem value
  | validSfToken value = Builder.byteString value
  | otherwise =
      Builder.word8 doubleQuote
        <> BS.foldr renderByte mempty value
        <> Builder.word8 doubleQuote
  where
    renderByte byte rest
      | byte == doubleQuote = Builder.byteString "\\\"" <> rest
      | byte == backslash = Builder.byteString "\\\\" <> rest
      | otherwise = Builder.word8 byte <> rest

foldMapWithSeparator :: Monoid m => m -> (a -> m) -> [a] -> m
foldMapWithSeparator _ _ [] = mempty
foldMapWithSeparator separator f (x : xs) = f x <> foldMap (separator <>) (map f xs)

acceptQueryParser :: Parser AcceptQuery
acceptQueryParser = do
  A.skipWhile (== space)
  firstRange <- mediaRangeParser
  remainingRanges <- many $ do
    skipOws
    A.word8 comma
    skipOws
    mediaRangeParser
  skipOws
  A.endOfInput
  pure $ mkAcceptQuery (firstRange :| remainingRanges)

mediaRangeParser :: Parser AcceptQueryMediaRange
mediaRangeParser = do
  bareRange <- bareItemParser
  rawParameters <- many parameterParser
  rangeValue <- requireText "Accept-Query list members must be Tokens or Strings" bareRange
  parameters <- traverse (requireText "Accept-Query parameter values must be Tokens or Strings") (Map.fromList rawParameters)
  case BS8.split '/' rangeValue of
    [type_, subtype] -> either (fail . Text.unpack) pure $ mkParsedMediaRange type_ subtype parameters
    _ -> fail "Invalid Accept-Query media range"

mkParsedMediaRange :: ByteString -> ByteString -> Map ByteString ByteString -> Either Text AcceptQueryMediaRange
mkParsedMediaRange type_ subtype parameters = do
  validateMediaRange type_ subtype
  pure
    MkAcceptQueryMediaRange
      { mediaRangeType = asciiLower type_
      , mediaRangeSubtype = asciiLower subtype
      , mediaRangeParameters = parameters
      }

parameterParser :: Parser (ByteString, BareItem)
parameterParser = do
  A.word8 semicolon
  A.skipWhile (== space)
  name <- parameterKeyParser
  value <- maybe UnsupportedBare id <$> optional (A.word8 equals *> bareItemParser)
  pure (name, value)

parameterKeyParser :: Parser ByteString
parameterKeyParser = do
  firstByte <- A.satisfy isKeyStart
  remaining <- A.takeWhile isKeyRest
  pure (BS.cons firstByte remaining)

-- Parsing unsupported bare-item types is necessary because RFC 9651 applies
-- duplicate-parameter last-wins semantics before Accept-Query's type
-- constraint. Their values are deliberately discarded instead of exposed as
-- a reusable Structured Fields syntax tree.
data BareItem
  = TextBare !ByteString
  | UnsupportedBare

requireText :: String -> BareItem -> Parser ByteString
requireText _ (TextBare value) = pure value
requireText message UnsupportedBare = fail message

bareItemParser :: Parser BareItem
bareItemParser = do
  firstByte <- A.peekWord8'
  case firstByte of
    byte
      | byte == doubleQuote -> TextBare <$> stringParser
      | isSfTokenStart byte -> TextBare <$> tokenParser
      | byte == hyphen || isDigitByte byte -> numberParser *> pure UnsupportedBare
      | byte == colon -> binaryParser *> pure UnsupportedBare
      | byte == questionMark -> booleanParser *> pure UnsupportedBare
      | byte == atSign -> dateParser *> pure UnsupportedBare
      | byte == percent -> displayStringParser *> pure UnsupportedBare
      | otherwise -> fail "Unsupported Structured Fields bare item"

stringParser :: Parser ByteString
stringParser = A.word8 doubleQuote *> go []
  where
    go bytes = do
      byte <- A.anyWord8
      case byte of
        _ | byte == doubleQuote -> pure . BS.pack $ reverse bytes
        _ | byte == backslash -> do
          escaped <- A.anyWord8
          unless (escaped == doubleQuote || escaped == backslash) $ fail "Invalid Structured Fields string escape"
          go (escaped : bytes)
        _ | isPrintableAscii byte -> go (byte : bytes)
        _ -> fail "Invalid Structured Fields string byte"

tokenParser :: Parser ByteString
tokenParser = do
  firstByte <- A.satisfy isSfTokenStart
  remaining <- A.takeWhile isSfTokenRest
  pure (BS.cons firstByte remaining)

numberParser :: Parser ()
numberParser = do
  _ <- optional (A.word8 hyphen)
  integral <- A.takeWhile1 isDigitByte
  fractional <- optional (A.word8 period *> A.takeWhile1 isDigitByte)
  case fractional of
    Nothing -> when (BS.length integral > 15) $ fail "Structured Fields integer is too long"
    Just digits -> do
      when (BS.length integral > 12) $ fail "Structured Fields decimal integral part is too long"
      when (BS.length digits > 3) $ fail "Structured Fields decimal fractional part is too long"

integerParser :: Parser ()
integerParser = do
  _ <- optional (A.word8 hyphen)
  digits <- A.takeWhile1 isDigitByte
  when (BS.length digits > 15) $ fail "Structured Fields integer is too long"

binaryParser :: Parser ()
binaryParser = do
  A.word8 colon
  encoded <- A.takeTill (== colon)
  A.word8 colon
  unless (validBase64 encoded) $ fail "Invalid Structured Fields byte sequence"

booleanParser :: Parser ()
booleanParser = A.word8 questionMark *> (A.word8 zero <|> A.word8 one) *> pure ()

dateParser :: Parser ()
dateParser = A.word8 atSign *> integerParser

displayStringParser :: Parser ()
displayStringParser = A.word8 percent *> A.word8 doubleQuote *> go []
  where
    go bytes = do
      byte <- A.anyWord8
      case byte of
        _ | byte == doubleQuote ->
          case Text.decodeUtf8' . BS.pack $ reverse bytes of
            Left _ -> fail "Invalid Structured Fields display string"
            Right _ -> pure ()
        _ | byte == percent -> do
          high <- A.anyWord8
          low <- A.anyWord8
          unless (isLowerHex high && isLowerHex low) $ fail "Invalid Structured Fields display string escape"
          go (hexByte high low : bytes)
        _ | isPrintableAscii byte -> go (byte : bytes)
        _ -> fail "Invalid Structured Fields display string byte"

validateMediaRange :: ByteString -> ByteString -> Either Text ()
validateMediaRange type_ subtype
  | BS.null type_ || BS.null subtype = Left "Empty Accept-Query media type or subtype"
  | not (BS.all isTChar type_ && BS.all isTChar subtype) = Left "Invalid Accept-Query media type or subtype"
  | type_ == "*" && subtype /= "*" = Left "A wildcard media type requires a wildcard subtype"
  | type_ /= "*" && BS.elem asterisk type_ = Left "Partial wildcards are not valid in an Accept-Query media type"
  | subtype /= "*" && BS.elem asterisk subtype = Left "Partial wildcards are not valid in an Accept-Query media subtype"
  | otherwise = Right ()

validSfToken :: ByteString -> Bool
validSfToken value =
  case BS.uncons value of
    Nothing -> False
    Just (firstByte, remaining) -> isSfTokenStart firstByte && BS.all isSfTokenRest remaining

validParameterKey :: ByteString -> Bool
validParameterKey value =
  case BS.uncons value of
    Nothing -> False
    Just (firstByte, remaining) -> isKeyStart firstByte && BS.all isKeyRest remaining

validBase64 :: ByteString -> Bool
validBase64 encoded =
  let (body, padding) = BS.spanEnd (== equals) encoded
      paddingLength = BS.length padding
      bodyLength = BS.length body
      totalLength = BS.length encoded
   in BS.all isBase64Byte body
        && paddingLength <= 2
        && bodyLength `mod` 4 /= 1
        && (paddingLength == 0 || totalLength `mod` 4 == 0)

asciiLower :: ByteString -> ByteString
asciiLower = BS8.map toLower

skipOws :: Parser ()
skipOws = A.skipWhile (\byte -> byte == space || byte == horizontalTab)

isPrintableAscii :: Word8 -> Bool
isPrintableAscii byte = byte >= space && byte <= tilde

isSfTokenStart :: Word8 -> Bool
isSfTokenStart byte = isAlphaByte byte || byte == asterisk

isSfTokenRest :: Word8 -> Bool
isSfTokenRest byte = isTChar byte || byte == colon || byte == slash

isKeyStart :: Word8 -> Bool
isKeyStart byte = isLowerAlphaByte byte || byte == asterisk

isKeyRest :: Word8 -> Bool
isKeyRest byte =
  isKeyStart byte
    || isDigitByte byte
    || byte == underscore
    || byte == hyphen
    || byte == period

isTChar :: Word8 -> Bool
isTChar byte =
  isAlphaByte byte
    || isDigitByte byte
    || byte `elem` tcharPunctuation

isAlphaByte :: Word8 -> Bool
isAlphaByte byte = isLowerAlphaByte byte || (byte >= capitalA && byte <= capitalZ)

isLowerAlphaByte :: Word8 -> Bool
isLowerAlphaByte byte = byte >= lowerA && byte <= lowerZ

isDigitByte :: Word8 -> Bool
isDigitByte byte = byte >= zero && byte <= nine

isBase64Byte :: Word8 -> Bool
isBase64Byte byte = isAlphaByte byte || isDigitByte byte || byte == plus || byte == slash

isLowerHex :: Word8 -> Bool
isLowerHex byte = isDigitByte byte || (byte >= lowerA && byte <= lowerF)

hexByte :: Word8 -> Word8 -> Word8
hexByte high low = hexValue high * 16 + hexValue low
  where
    hexValue byte
      | isDigitByte byte = byte - zero
      | otherwise = byte - lowerA + 10

tcharPunctuation :: [Word8]
tcharPunctuation = [33, 35, 36, 37, 38, 39, 42, 43, 45, 46, 94, 95, 96, 124, 126]

space, horizontalTab, doubleQuote, percent, asterisk, plus, comma, hyphen, period, slash, colon, semicolon, equals, questionMark, atSign, backslash, underscore, tilde :: Word8
space = 32
horizontalTab = 9
doubleQuote = 34
percent = 37
asterisk = 42
plus = 43
comma = 44
hyphen = 45
period = 46
slash = 47
colon = 58
semicolon = 59
equals = 61
questionMark = 63
atSign = 64
backslash = 92
underscore = 95
tilde = 126

zero, one, nine, capitalA, capitalZ, lowerA, lowerF, lowerZ :: Word8
zero = 48
one = 49
nine = 57
capitalA = 65
capitalZ = 90
lowerA = 97
lowerF = 102
lowerZ = 122
