{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
module Servant.Common.BaseUrl where

import Data.List
import GHC.Generics
import Network.URI
import Safe
import Text.Read

data Scheme = Http | Https
  deriving (Show, Eq, Ord, Generic)

data BaseUrl = BaseUrl {
  baseUrlScheme :: Scheme,
  baseUrlHost :: String,
  baseUrlPort :: Int
 }
  deriving (Show, Eq, Ord, Generic)

showBaseUrl :: BaseUrl -> String
showBaseUrl (BaseUrl scheme host port) =
  schemeString ++ "//" ++ host ++ portString
    where
      schemeString = case scheme of
        Http  -> "http:"
        Https -> "https:"
      portString = case (scheme, port) of
        (Http, 80) -> ""
        (Https, 443) -> ""
        _ -> ":" ++ show port

parseBaseUrl :: String -> Either String BaseUrl
parseBaseUrl s = case parseURI (removeTrailingSlash s) of
  -- This is a rather hacky implementation and should be replaced with something
  -- implemented in attoparsec (which is already a dependency anyhow (via aeson)).
  Just (URI "http:" (Just (URIAuth "" host (':' : (readMaybe -> Just port)))) "" "" "") ->
    Right (BaseUrl Http host port)
  Just (URI "http:" (Just (URIAuth "" host "")) "" "" "") ->
    Right (BaseUrl Http host 80)
  Just (URI "https:" (Just (URIAuth "" host (':' : (readMaybe -> Just port)))) "" "" "") ->
    Right (BaseUrl Https host port)
  Just (URI "https:" (Just (URIAuth "" host "")) "" "" "") ->
    Right (BaseUrl Https host 443)
  _ -> if "://" `isInfixOf` s
    then Left ("invalid base url: " ++ s)
    else parseBaseUrl ("http://" ++ s)
 where
  removeTrailingSlash s = case lastMay s of
    Just '/' -> init s
    _ -> s
