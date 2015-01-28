{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
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
module Servant.QQ (sitemap) where

import Control.Applicative ( (<$>) )
import Control.Monad ( void )
import Data.Monoid ( Monoid(..), (<>) )
import Data.Maybe ( mapMaybe )
import Language.Haskell.TH.Quote ( QuasiQuoter(..) )
import Language.Haskell.TH
    ( mkName, Type(AppT, ConT, LitT), TyLit(StrTyLit) )
import Text.ParserCombinators.Parsec
    ( try,
      Parser,
      manyTill,
      endBy,
      sepBy1,
      optional,
      optionMaybe,
      string,
      anyChar,
      char,
      spaces,
      noneOf,
      parse,
      skipMany,
      many,
      lookAhead,
      (<|>),
      (<?>) )
import Servant.API.Capture ( Capture )
import Servant.API.Get ( Get )
import Servant.API.Post ( Post )
import Servant.API.Put ( Put )
import Servant.API.Delete ( Delete )
import Servant.API.QueryParam ( QueryParam )
import Servant.API.MatrixParam ( MatrixParam )
import Servant.API.ReqBody ( ReqBody )
import Servant.API.Sub ( (:>) )
import Servant.API.Alternative ( (:<|>) )

data MethodE
data PathElemE
data OptsE
data SitemapE


data Validation e a = Failure e
                    | Success a

instance Functor (Validation e) where
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success (f a)

data Exp a where
    Method   :: String -> Exp MethodE
    Slash    :: Exp PathElemE -> Exp PathElemE -> Exp PathElemE
    PathElem :: String -> Exp PathElemE
    Opts     :: String -> Exp OptsE
    JoinOpts :: Exp OptsE -> Exp OptsE -> Exp OptsE
    AddOpts  :: Exp SitemapE -> Exp OptsE -> Exp SitemapE
    Line     :: Exp MethodE -> Exp PathElemE -> Exp SitemapE
    Sitemap  :: Exp SitemapE -> Exp SitemapE -> Exp SitemapE

data ParseError = ParseError Int String

parseEverything :: String -> Exp SitemapE
parseEverything str = removeComments <$> lines str
    where removeComments = takeWhile (/= '#')
          parseLines _  [] = []
          parseLines lineno (x:xs) = case opts of
                                         [] -> (parseUrlLine x):parseLines rest
                                         xs -> (parseUrlLine x `AddOpts`
            where (opts, rest) = span (startsWith ' ') xs



parsePath :: String -> Exp PathElemE
parsePath line = foldr1 Slash $ PathElem <$> wordsBy '/' line

parseOpts :: Int -> [String] -> Either ParseError (Exp OptsE)
parseOpts lineno lines = foldr1 JoinOpts $ Opts <$> lines

parseUrlLine :: Int -> String -> Either ParseError (Exp SitemapE)
parseUrlLine lineno line = case span '>' line of
    ([], x) ->  Left $ ParseError lineno "Expected method and url before '>'"
    (xs, ">") -> Left $ ParseError lineno "Expected type after '>'"
    (xs, '>':ys) | length (words xs) /= 2 -> Left $ ParseError lineno "Expected method and url before '>'"
                 | otherwise -> Line (Method met) (parsePath url)
                     where met:url = words xs

wordsBy :: Char -> String -> [String]
wordsBy c s =  case dropWhile (== c) s of
    "" -> []
    s' -> w : words s''
          where (w, s'') = break (== c) s'


data SitemapParser = SitemapParser
                   { methodParsers :: [String -> Maybe Type]
                   , pathParsers   :: [String -> Maybe Type]
                   , optsParsers   :: [String -> Maybe Type]
                   }


pFirstWE :: [a -> Maybe b] -> a -> c -> Validation c b
pFirstWE fn a c = maybe (Failure c) Success $ pFirst fn a
    where pFirst :: [a -> Maybe b] -> a -> Maybe b
          pFirst fns s = case mapMaybe ($ s) fns of
                           [] -> Nothing
                           x:_ -> Just x


joinWith :: Monoid err => (typ -> typ -> typ)
                       -> Validation err typ
                       -> Validation err typ
                       -> Validation err typ
joinWith mult (Success s1) (Success s2) = Success (s1 `mult` s2)
joinWith _    (Failure e1) (Failure e2) = Failure (e1 <> e2)
joinWith _    (Failure e1) _            = Failure e1
joinWith _    _            (Failure e2) = Failure e2

pathUnion :: Type -> Type -> Type
pathUnion a = AppT (AppT (ConT ''(:>)) a)

optsUnion :: Type -> Type -> Type
optsUnion a = AppT (AppT (ConT ''(:<|>)) a)

data UndefinedError = UndefinedPath String
                    | UndefinedMethod String
                    | UndefinedOpts String
                    deriving (Eq, Show)

evalPath :: SitemapParser -> Exp PathElemE -> Validation [UndefinedError] Type
evalPath SitemapParser{..} (PathElem path) = pFirstWE pathParsers path [UndefinedPath path]
evalPath sp (p1 `Slash` p2) = joinWith pathUnion (evalPath sp p1) (evalPath sp p2)

evalMethod :: SitemapParser -> Exp MethodE -> Validation [UndefinedError] Type
evalMethod SitemapParser{..} (Method met) = pFirstWE methodParsers met [UndefinedMethod met]

evalOpts :: SitemapParser -> Exp OptsE -> Validation [UndefinedError] Type
evalOpts SitemapParser{..} (Opts opt) = pFirstWE optsParsers opt [UndefinedOpts opt]
evalOpts sp (o1 `JoinOpts` o2) = joinWith optsUnion (evalOpts sp o1) (evalOpts sp o2)


addMethodParser, addPathParser, addOptsParser :: (String -> Maybe Type) -> SitemapParser -> SitemapParser
addMethodParser a x@SitemapParser{..} = x{ methodParsers = methodParsers ++ [a] }
addOptsParser a x@SitemapParser{..} = x{ optsParsers = optsParsers ++ [a] }
addPathParser a x@SitemapParser{..} = x{ pathParsers = pathParsers ++ [a] }

-- | Finally-tagless encoding for our DSL.
-- Keeping 'repr'' and 'repr' distinct when writing functions with an
-- @ExpSYM@ context ensures certain invariants (for instance, that there is
-- only one of 'get', 'post', 'put', and 'delete' in a value), but
-- sometimes requires a little more work.
class ExpSYM repr' repr | repr -> repr', repr' -> repr where
    lit         :: String -> repr' -> repr
    capture     :: String -> String -> repr -> repr
    reqBody     :: String -> repr -> repr
    queryParam  :: String -> String -> repr -> repr
    matrixParam :: String -> String -> repr -> repr
    conj        :: repr' -> repr -> repr
    get         :: String -> repr
    post        :: String -> repr
    put         :: String -> repr
    delete      :: String -> repr


infixr 6 >:

(>:) :: Type -> Type -> Type
(>:) = conj


instance ExpSYM Type Type where
    lit name r         = LitT (StrTyLit name) >: r
    capture name typ r = AppT (AppT (ConT ''Capture) (LitT (StrTyLit name)))
                               (ConT $ mkName typ) >: r
    reqBody typ r      = AppT (ConT ''ReqBody) (ConT $ mkName typ) >: r
    queryParam name typ r  = AppT (AppT (ConT ''QueryParam) (LitT (StrTyLit name)))
                               (ConT $ mkName typ) >: r
    matrixParam name typ r = AppT (AppT (ConT ''MatrixParam) (LitT (StrTyLit name)))
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
         cname <- many (noneOf " ?/:;")
         char ':'
         ctyp  <- many (noneOf " ?/:;")
         mx <- many parseMatrixParam
         return $ capture cname ctyp . foldr (.) id mx
      parseQueryParam = do
         char '?'
         cname <- many (noneOf " ?/:;")
         char ':'
         ctyp  <- many (noneOf " ?/:;")
         return $ queryParam cname ctyp
      parseLit = do
         lt <- many (noneOf " ?/:;")
         mx <- many parseMatrixParam
         return $ lit lt . foldr (.) id mx
      parseMatrixParam = do
         char ';'
         cname <- many (noneOf " ?/:;")
         char ':'
         ctyp  <- many (noneOf " ?/:;")
         return $ matrixParam cname ctyp

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
