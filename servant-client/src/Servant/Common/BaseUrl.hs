{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ViewPatterns       #-}
module Servant.Common.BaseUrl (
  -- * types
    BaseUrl (..)
  , InvalidBaseUrlException
  , Scheme (..)
  -- * functions
  , parseBaseUrl
  , showBaseUrl
) where

import           Control.Monad.Catch (Exception, MonadThrow, throwM)
import           Data.List
import           Data.Typeable
import           GHC.Generics
import           Network.URI hiding (path)
import           Safe
import           Text.Read

-- | URI scheme to use
data Scheme =
    Http  -- ^ http://
  | Https -- ^ https://
  deriving (Show, Eq, Ord, Generic)

-- | Simple data type to represent the target of HTTP requests
--   for servant's automatically-generated clients.
data BaseUrl = BaseUrl
  { baseUrlScheme :: Scheme -- ^ URI scheme to use
  , baseUrlHost   :: String   -- ^ host (eg "haskell.org")
  , baseUrlPort   :: Int      -- ^ port (eg 80)
  , baseUrlPath   :: String   -- ^ path (eg "/a/b/c")
  } deriving (Show, Ord, Generic)

instance Eq BaseUrl where
    BaseUrl a b c path == BaseUrl a' b' c' path'
        = a == a' && b == b' && c == c' && s path == s path'
        where s ('/':x) = x
              s x       = x

showBaseUrl :: BaseUrl -> String
showBaseUrl (BaseUrl urlscheme host port path) =
  schemeString ++ "//" ++ host ++ (portString </> path)
    where
      a </> b = if "/" `isPrefixOf` b || null b then a ++ b else a ++ '/':b
      schemeString = case urlscheme of
        Http  -> "http:"
        Https -> "https:"
      portString = case (urlscheme, port) of
        (Http, 80) -> ""
        (Https, 443) -> ""
        _ -> ":" ++ show port

data InvalidBaseUrlException = InvalidBaseUrlException String deriving (Show, Typeable)
instance Exception InvalidBaseUrlException

parseBaseUrl :: MonadThrow m => String -> m BaseUrl
parseBaseUrl s = case parseURI (removeTrailingSlash s) of
  -- This is a rather hacky implementation and should be replaced with something
  -- implemented in attoparsec (which is already a dependency anyhow (via aeson)).
  Just (URI "http:" (Just (URIAuth "" host (':' : (readMaybe -> Just port)))) path "" "") ->
    return (BaseUrl Http host port path)
  Just (URI "http:" (Just (URIAuth "" host "")) path "" "") ->
    return (BaseUrl Http host 80 path)
  Just (URI "https:" (Just (URIAuth "" host (':' : (readMaybe -> Just port)))) path "" "") ->
    return (BaseUrl Https host port path)
  Just (URI "https:" (Just (URIAuth "" host "")) path "" "") ->
    return (BaseUrl Https host 443 path)
  _ -> if "://" `isInfixOf` s
    then throwM (InvalidBaseUrlException $ "Invalid base URL: " ++ s)
    else parseBaseUrl ("http://" ++ s)
 where
  removeTrailingSlash str = case lastMay str of
    Just '/' -> init str
    _ -> str
