{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Servant.API.QQ where

import Control.Applicative
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Safe (headMay)

import Servant.API.Capture
import Servant.API.Get
import Servant.API.Post
import Servant.API.Put
import Servant.API.Delete
import Servant.API.QueryParam
import Servant.API.ReqBody
import Servant.API.Sub
import Servant.API.Union

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

readEntry :: ExpSYM r r => [String] -> Maybe r
readEntry []     = Nothing
readEntry (met:xs:typ) = case met of
    "GET"    -> rd get
    "POST"   -> rd post
    "PUT"    -> rd put
    "DELETE" -> rd delete
    x        -> error $ "Unknown method: " ++ x
  where typ' = splitOn "->" $ concat typ
        rd m = case typ' of
            []           -> readEntry' xs $ m "()"
            [rsp]        -> readEntry' xs $ m rsp
            (rqbd:[rsp]) -> readEntry' xs $ reqBody rqbd $ m rsp
            _            -> error "Only functions of one argument allowed!"
readEntry x    = error $ "Wrong number of elems in line: " ++ show x

readEntry' :: ExpSYM r r => String -> r -> Maybe r
readEntry' []   _ = Nothing
readEntry' xs r = Just $ foldr1 (.) (tRepr <$> splitOn "/" xs) r
    where
          tRepr y | [x] <- splitOn ":" y   = lit x
                  | a:[b] <- splitOn ":" y = case headMay a of
                                                 Just '?' -> queryParam (tail a) b
                                                 Just _   -> capture a b
                                                 Nothing  -> error "Expecting something after '/'"
                  | otherwise              = error "Only one ':' per section"

readAll :: String -> Type
readAll s = foldr1 union $ mapMaybe readEntry $ words <$> lines s
   where union :: Type -> Type -> Type
         union a = AppT (AppT (ConT ''(:<|>)) a)

sitemap :: QuasiQuoter
sitemap = QuasiQuoter { quoteExp = undefined
                      , quotePat = undefined
                      , quoteType = return . readAll
                      , quoteDec = undefined
                      }

