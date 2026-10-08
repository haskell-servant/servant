{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.API.AcceptQuerySpec where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import Test.Hspec

import Servant.API (parseHeader, toHeader)
import Servant.API.AcceptQuery
import Servant.API.ResponseHeaders (Headers, addHeader, getHeaders)

spec :: Spec
spec = describe "Servant.API.AcceptQuery" $ do
  describe "parsing" $ do
    it "parses the RFC 10008 example" $ do
      let accepted = parseAcceptQuery' "\"application/jsonpath\", application/sql;charset=\"UTF-8\""
          ranges = NE.toList (acceptQueryMediaRanges accepted)
      map mediaRangeBase ranges
        `shouldBe` [("application", "jsonpath"), ("application", "sql")]
      toHeader accepted
        `shouldBe` "application/jsonpath, application/sql;charset=UTF-8"

    it "treats token and quoted media members as equivalent" $
      parseAcceptQuery' "application/sql, \"application/jsonpath\""
        `shouldBe` parseAcceptQuery' "\"application/sql\", application/jsonpath"

    it "accepts media ranges requiring a quoted structured-field string" $
      toHeader (parseAcceptQuery' "\"123/example\"")
        `shouldBe` "\"123/example\""

    it "accepts the RFC wildcard forms" $ do
      let ranges = NE.toList . acceptQueryMediaRanges $ parseAcceptQuery' "*/*, application/*"
      map mediaRangeBase ranges `shouldBe` [("*", "*"), ("application", "*")]

    it "treats token and string parameter values as equivalent" $
      parseAcceptQuery' "application/sql;charset=UTF-8"
        `shouldBe` parseAcceptQuery' "application/sql;charset=\"UTF-8\""

    it "accepts structured-field whitespace where RFC 9651 permits it" $
      parseAcceptQuery' "  application/sql; charset=UTF-8 \t, \t\"application/jsonpath\"\t"
        `shouldBe` parseAcceptQuery' "application/sql;charset=UTF-8, application/jsonpath"

    it "keeps the last duplicate parameter" $
      toHeader (parseAcceptQuery' "application/sql;foo=first;foo=second")
        `shouldBe` "application/sql;foo=second"

  describe "normalization" $ do
    it "makes media-range order semantically insignificant" $ do
      let sql = mediaRange "application" "sql"
          json = mediaRange "application" "json"
          reverseOrder = mkAcceptQuery (sql :| [json])
          canonicalOrder = mkAcceptQuery (json :| [sql])
      reverseOrder `shouldBe` canonicalOrder
      toHeader reverseOrder `shouldBe` "application/json, application/sql"

    it "canonicalizes case-insensitive media components and parameter names, not values" $ do
      let mixedCase = expectRight $ mkAcceptQueryMediaRange "Application" "SQL" [("Charset", "UTF-8")]
          canonical = expectRight $ mkAcceptQueryMediaRange "application" "sql" [("charset", "UTF-8")]
          distinctValue = expectRight $ mkAcceptQueryMediaRange "application" "sql" [("charset", "utf-8")]
      mixedCase `shouldBe` canonical
      mixedCase `shouldNotBe` distinctValue
      toHeader (mkAcceptQuery (mixedCase :| [])) `shouldBe` "application/sql;charset=UTF-8"

    it "renders media members and parameters canonically" $ do
      let range = expectRight $ mkAcceptQueryMediaRange "application" "sql" [("z", "quoted value"), ("a", "token")]
      toHeader (mkAcceptQuery (range :| []))
        `shouldBe` "application/sql;a=token;z=\"quoted value\""

    it "round-trips rendered values semantically"
      $ forM_
        ( [ "application/sql"
          , "\"123/example\";charset=\"UTF-8\""
          , "*/*"
          , "application/*"
          , "application/sql;charset=utf-8, \"application/jsonpath\""
          ]
            :: [ByteString]
        )
      $ \input -> do
        let accepted = parseAcceptQuery' input
        parseAcceptQuery (toHeader accepted) `shouldBe` Right accepted

    it "exposes media-range components and parameters" $ do
      let range = onlyMediaRange $ parseAcceptQuery' "application/sql;charset=UTF-8"
      acceptQueryType range `shouldBe` "application"
      acceptQuerySubtype range `shouldBe` "sql"
      mkAcceptQueryMediaRange
        (acceptQueryType range)
        (acceptQuerySubtype range)
        (acceptQueryParameters range)
        `shouldBe` Right range

  describe "invalid values" $
    forM_ malformedAcceptQueries $ \(description, input) ->
      it ("rejects " <> description) $
        parseAcceptQuery input `shouldSatisfy` isLeft

  describe "response headers" $
    it "renders an Accept-Query header through addHeader and getHeaders" $ do
      let accepted = parseAcceptQuery' "\"application/jsonpath\", application/sql;charset=\"UTF-8\""
          response = addHeader accepted True :: Headers '[AcceptQueryHeader] Bool
      getHeaders response
        `shouldBe` [(hAcceptQuery, "application/jsonpath, application/sql;charset=UTF-8")]

parseAcceptQuery :: ByteString -> Either Text AcceptQuery
parseAcceptQuery = parseHeader

parseAcceptQuery' :: ByteString -> AcceptQuery
parseAcceptQuery' = expectRight . parseAcceptQuery

mediaRange :: ByteString -> ByteString -> AcceptQueryMediaRange
mediaRange type_ subtype = expectRight $ mkAcceptQueryMediaRange type_ subtype mempty

mediaRangeBase :: AcceptQueryMediaRange -> (ByteString, ByteString)
mediaRangeBase range = (acceptQueryType range, acceptQuerySubtype range)

onlyMediaRange :: AcceptQuery -> AcceptQueryMediaRange
onlyMediaRange = NE.head . acceptQueryMediaRanges

expectRight :: Show e => Either e a -> a
expectRight = either (error . show) id

malformedAcceptQueries :: [(String, ByteString)]
malformedAcceptQueries =
  [ ("an empty list", "")
  , ("a leading separator", ",application/sql")
  , ("a trailing separator", "application/sql,")
  , ("repeated separators", "application/sql,,application/json")
  , ("an unterminated media string", "\"application/sql")
  , ("an unterminated parameter string", "application/sql;charset=\"UTF-8")
  , ("an invalid string escape", "\"application\\q\"")
  , ("a dangling string escape", "\"application\\")
  , ("a control octet", BS.singleton 0)
  , ("a non-ASCII octet", BS.singleton 0x80)
  , ("an integer item", "1")
  , ("a decimal item", "1.5")
  , ("a Boolean item", "?1")
  , ("a byte sequence item", ":YWJj:")
  , ("a date item", "@1")
  , ("a display string item", "%\"application/sql\"")
  , ("an inner list", "(application/sql)")
  , ("an empty media range", "\"\"")
  , ("a media range without a slash", "application")
  , ("a media range without a type", "\"/sql\"")
  , ("a media range without a subtype", "application/")
  , ("a media range with extra slashes", "application//sql")
  , ("a wildcard type with a concrete subtype", "*/sql")
  , ("a partial wildcard subtype", "application/j*son")
  , ("a partial wildcard type", "app*/sql")
  , ("an uppercase parameter key", "application/sql;Charset=utf-8")
  , ("an invalid parameter key", "application/sql;1charset=utf-8")
  , ("a missing parameter value", "application/sql;charset=")
  , ("an implicit Boolean parameter", "application/sql;charset")
  , ("an integer parameter", "application/sql;charset=1")
  , ("a Boolean parameter", "application/sql;charset=?1")
  , ("a byte sequence parameter", "application/sql;charset=:YWJj:")
  , ("trailing input", "application/sql trailing")
  , ("leading horizontal whitespace", "\tapplication/sql")
  , ("whitespace before a parameter", "application/sql ;charset=utf-8")
  , ("a tab after a parameter separator", "application/sql;\tcharset=utf-8")
  , ("whitespace around a parameter equals sign", "application/sql;charset =utf-8")
  ]
