{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-- | QuasiQuoting utilities for API types.
--
-- 'sitemap' allows you to write your type in a very natural way:
--
-- @
-- [sitemap|
-- PUT        hello                 String -> ()
-- POST       hello/p:Int           String -> ()
-- GET        hello/?name:String    Int
-- |]
-- @
--
-- Will generate:
--
-- @
--        "hello" :> ReqBody String :> Put ()
--   :\<|> "hello" :> Capture "p" Int :> ReqBody String :> Post ()
--   :\<|> "hello" :> QueryParam "name" String :> Get Int
-- @
--
-- Note the @/@ before a @QueryParam@!
module Servant.QQ where

import Control.Monad (void)
import Control.Applicative hiding (many, (<|>), optional)
import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Text.ParserCombinators.Parsec

import Servant.API.Capture
import Servant.API.Get
import Servant.API.Post
import Servant.API.Put
import Servant.API.Delete
import Servant.API.QueryParam
import Servant.API.ReqBody
import Servant.API.Sub
import Servant.API.Alternative

-- | Finally-tagless encoding for our DSL.
-- Keeping 'repr'' and 'repr' distinct when writing functions with an
-- @ExpSYM@ context ensures certain invariants (for instance, that there is
-- only one of 'get', 'post', 'put', and 'delete' in a value), but
-- sometimes requires a little more work.
class ExpSYM repr' repr | repr -> repr', repr' -> repr where
    lit        :: String -> repr' -> repr
    capture    :: String -> String -> repr -> repr
    reqBody    :: String -> repr -> repr
    queryParam :: String -> String -> repr -> repr
    conj       :: repr' -> repr -> repr
    get        :: String -> repr
    post       :: String -> repr
    put        :: String -> repr
    delete     :: String -> repr


infixr 6 >:

(>:) :: Type -> Type -> Type
(>:) = conj


instance ExpSYM Type Type where
    lit name r         = LitT (StrTyLit name) >: r
    capture name typ r = AppT (AppT (ConT ''Capture) (LitT (StrTyLit name)))
                               (ConT $ mkName typ) >: r
    reqBody typ r      = AppT (ConT ''ReqBody) (ConT $ mkName typ) >: r
    queryParam name typ r = AppT (AppT (ConT ''QueryParam) (LitT (StrTyLit name)))
                               (ConT $ mkName typ) >: r
    conj x             = AppT (AppT (ConT ''(:>)) x)
    get  typ           = AppT (ConT ''Get) (ConT $ mkName typ)
    post typ           = AppT (ConT ''Post) (ConT $ mkName typ)
    put typ            = AppT (ConT ''Put) (ConT $ mkName typ)
    delete "()"        = ConT ''Delete
    delete _           = error "Delete does not return a request body"

parseMethod :: ExpSYM repr' repr => Parser (String -> repr)
parseMethod = try (string "GET"    >> return get)
          <|> try (string "POST"   >> return post)
          <|> try (string "PUT"    >> return put)
          <|> try (string "DELETE" >> return delete)

parseUrlSegment :: ExpSYM repr repr => Parser (repr -> repr)
parseUrlSegment = try parseCapture
              <|> try parseQueryParam
              <|> try parseLit
  where
      parseCapture = do
         cname <- many (noneOf " ?/:")
         char ':'
         ctyp  <- many (noneOf " ?/:")
         return $ capture cname ctyp
      parseQueryParam = do
         char '?'
         cname <- many (noneOf " ?/:")
         char ':'
         ctyp  <- many (noneOf " ?/:")
         return $ queryParam cname ctyp
      parseLit = lit <$> many (noneOf " ?/:")

parseUrl :: ExpSYM repr repr => Parser (repr -> repr)
parseUrl = do
    optional $ char '/'
    url <- parseUrlSegment `sepBy1` char '/'
    return $ foldr1 (.) url

data Typ = Val String
         | ReqArgVal String String

parseTyp :: Parser Typ
parseTyp = do
    f <- many (noneOf "-{\n\r")
    spaces
    s <- optionMaybe (try parseRet)
    try $ optional inlineComment
    try $ optional blockComment
    case s of
        Nothing -> return $ Val (stripTr f)
        Just s' -> return $ ReqArgVal (stripTr f) (stripTr s')
  where
    parseRet :: Parser String
    parseRet = do
        string "->"
        spaces
        many (noneOf "-{\n\r")
    stripTr = reverse . dropWhile (== ' ') . reverse


parseEntry :: ExpSYM repr repr => Parser repr
parseEntry = do
    met <- parseMethod
    spaces
    url <- parseUrl
    spaces
    typ <- parseTyp
    case typ of
        Val s -> return $ url (met s)
        ReqArgVal i o -> return $ url $ reqBody i (met o)

blockComment :: Parser ()
blockComment = do
    string "{-"
    manyTill anyChar (try $ string "-}")
    return ()

inlineComment :: Parser ()
inlineComment = do
    string "--"
    manyTill anyChar (try $ lookAhead eol)
    return ()

eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"

eols :: Parser ()
eols = skipMany $ void eol <|> blockComment <|> inlineComment

parseAll :: Parser Type
parseAll = do
    eols
    entries <- parseEntry `endBy` eols
    return $ foldr1 union entries
  where union :: Type -> Type -> Type
        union a = AppT (AppT (ConT ''(:<|>)) a)

-- | The sitemap QuasiQuoter.
--
--     * @.../<var>:<type>/...@ becomes a capture
--     * @.../?<var>:<type>@ becomes a query parameter
--     * @<method>   ...  <typ>@ becomes a method returning @<typ>@
--     * @<method>   ...  <typ1> -> <typ2>@ becomes a method with request
--       body of @<typ1>@ and returning @<typ2>@
--
-- Comments are allowed, and have the standard Haskell format
--
--     * @--@ for inline
--     * @{- ... -}@ for block
--
sitemap :: QuasiQuoter
sitemap = QuasiQuoter { quoteExp = undefined
                      , quotePat = undefined
                      , quoteType = \x -> case parse parseAll "" x of
                            Left err -> error $ show err
                            Right st -> return st
                      , quoteDec = undefined
                      }

