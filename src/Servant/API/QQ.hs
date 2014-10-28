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

import Servant.API.Capture
import Servant.API.Get
import Servant.API.Post
import Servant.API.Put
import Servant.API.Delete
import Servant.API.RQBody
import Servant.API.Sub
import Servant.API.Union

class ExpSYM repr' repr | repr -> repr', repr' -> repr where
    lit        :: String -> repr' -> repr
    capture    :: String -> String -> repr -> repr
    rqBody     :: String -> repr -> repr
    conj       :: repr' -> repr -> repr
    get        :: String -> repr
    post       :: String -> repr
    put        :: String -> repr
    delete     :: String -> repr

infixr 6 >:

(>:) :: Type -> Type -> Type
(>:) = conj


instance ExpSYM Type Type where
    lit name r         = (LitT (StrTyLit name)) >: r
    capture name typ r = (AppT (AppT (ConT ''Capture) (LitT (StrTyLit name)))
                               (ConT $ mkName typ)) >: r
    rqBody typ r     = (AppT (ConT ''RQBody) (ConT $ mkName typ)) >: r
    conj x y         = AppT (AppT (ConT ''(:>)) x) y
    get  typ         = AppT (ConT ''Get) (ConT $ mkName typ)
    post typ         = AppT (ConT ''Post) (ConT $ mkName typ)
    put typ          = AppT (ConT ''Put) (ConT $ mkName typ)
    delete "()"      = ConT ''Delete
    delete _         = error "Delete does not return a request body"

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
            (rqbd:[rsp]) -> readEntry' xs $ rqBody rqbd $ m rsp
            _            -> error "Only functions of one argument allowed!"
readEntry x    = error $ "Wrong number of elems in line: " ++ show x

readEntry' :: ExpSYM r r => String -> r -> Maybe r
readEntry' []   _ = Nothing
readEntry' xs r = Just $ foldr1 (.) (tRepr <$> splitOn "/" xs) r
    where
          tRepr y | [x] <- splitOn ":" y   = lit x
                  | a:[b] <- splitOn ":" y = capture a b
                  | otherwise              = error "Only one ':' per section"

readAll :: String -> Type
readAll s = foldr1 union $ mapMaybe readEntry $ words <$> lines s
   where union :: Type -> Type -> Type
         union a b = AppT (AppT (ConT ''(:<|>)) a) b

sitemap :: QuasiQuoter
sitemap = QuasiQuoter { quoteExp = undefined
                      , quotePat = undefined
                      , quoteType = return . readAll
                      , quoteDec = undefined
                      }

