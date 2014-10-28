{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Servant.API.QQ where

import Control.Applicative
import Data.List.Split (splitOn)
--import Language.Haskell.TH.Quote
import Language.Haskell.TH

import Servant.API.Capture
import Servant.API.Get
import Servant.API.Post
import Servant.API.Put
import Servant.API.Delete
import Servant.API.Sub

class ExpSYM repr' repr | repr -> repr', repr' -> repr where
    simplePath :: String -> repr'
    capture    :: String -> String -> repr'
    conj       :: repr' -> repr' -> repr'
    get        :: String -> repr' -> repr
    post       :: String -> repr' -> repr
    put        :: String -> repr' -> repr
    delete     :: String -> repr' -> repr

instance ExpSYM Type Type where
    simplePath name  = LitT (StrTyLit name)
    capture name typ = AppT (AppT (ConT ''Capture) (simplePath name))
                            (ConT $ mkName typ)
    conj f s         = AppT (AppT (ConT ''(:>)) f) s
    get  typ r       = AppT (AppT (ConT ''(:>)) r)
                            (AppT (ConT ''Get) (ConT $ mkName typ))
    post typ r       = AppT (AppT (ConT ''(:>)) r)
                            (AppT (ConT ''Post) (ConT $ mkName typ))
    put typ r        = AppT (AppT (ConT ''(:>)) r)
                            (AppT (ConT ''Put) (ConT $ mkName typ))
    delete typ r     = AppT (AppT (ConT ''(:>)) r)
                            (AppT (ConT ''Delete) (ConT $ mkName typ))

readEntry :: ExpSYM r' r => [String] -> Maybe r
readEntry []     = Nothing
readEntry (met:typ:xs) = case met of
    "GET"    -> get  typ <$> readEntry' xs
    "POST"   -> post typ <$> readEntry' xs
    "PUT"    -> put  typ <$> readEntry' xs
    "DELETE" -> delete typ <$> readEntry' xs
    x        -> error $ "Unknown method: " ++ x
readEntry x    = error $ "Wrong number of elems in line: " ++ show x

readEntry' :: ExpSYM r' r => [String] -> Maybe r'
readEntry' [] = Nothing
readEntry' [xs] = Just $ foldr1 conj $ tRepr <$> splitOn "/" xs
    where tRepr y | [x] <- splitOn ":" y   = simplePath x
                  | x:[y] <- splitOn ":" y = capture x y
                  | otherwise              = error "Only one ':' per section"
readEntry' _ = Nothing

