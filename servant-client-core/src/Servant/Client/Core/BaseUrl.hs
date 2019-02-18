{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE ViewPatterns       #-}
module Servant.Client.Core.BaseUrl (
    BaseUrl (..),
    Scheme (..),
    showBaseUrl,
    parseBaseUrl,
    InvalidBaseUrlException (..),
    ) where

import           Control.DeepSeq
                 (NFData (..))
import           Control.Monad.Catch
                 (Exception, MonadThrow, throwM)
import           Data.Aeson
                 (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..))
import           Data.Aeson.Types
                 (FromJSONKeyFunction (..), contramapToJSONKeyFunction,
                 withText)
import           Data.Data
                 (Data)
import           Data.List
import qualified Data.Text                  as T
import           GHC.Generics
import           Language.Haskell.TH.Syntax
                 (Lift)
import           Network.URI                hiding
                 (path)
import           Safe
import           Text.Read

-- | URI scheme to use
data Scheme =
    Http  -- ^ http://
  | Https -- ^ https://
  deriving (Show, Eq, Ord, Generic, Lift, Data)

-- | Simple data type to represent the target of HTTP requests
--   for servant's automatically-generated clients.
data BaseUrl = BaseUrl
  { baseUrlScheme :: Scheme   -- ^ URI scheme to use
  , baseUrlHost   :: String   -- ^ host (eg "haskell.org")
  , baseUrlPort   :: Int      -- ^ port (eg 80)
  , baseUrlPath   :: String   -- ^ path (eg "/a/b/c")
  } deriving (Show, Ord, Generic, Lift, Data)
-- TODO: Ord is more precise than Eq
-- TODO: Add Hashable instance?
--
instance NFData BaseUrl where
  rnf (BaseUrl a b c d) = a `seq` rnf b `seq` rnf c `seq` rnf d

instance Eq BaseUrl where
    BaseUrl a b c path == BaseUrl a' b' c' path'
        = a == a' && b == b' && c == c' && s path == s path'
        where s ('/':x) = x
              s x       = x

-- | >>> traverse_ (LBS8.putStrLn . encode) $ parseBaseUrl "api.example.com"
-- "http://api.example.com"
instance ToJSON BaseUrl where
    toJSON     = toJSON . showBaseUrl
    toEncoding = toEncoding . showBaseUrl

-- | >>> parseBaseUrl "api.example.com" >>= decode . encode :: Maybe BaseUrl
-- Just (BaseUrl {baseUrlScheme = Http, baseUrlHost = "api.example.com", baseUrlPort = 80, baseUrlPath = ""})
instance FromJSON BaseUrl where
    parseJSON = withText "BaseUrl" $ \t -> case parseBaseUrl (T.unpack t) of
        Just u  -> return u
        Nothing -> fail $ "Invalid base url: " ++ T.unpack t

-- | >>> :{
-- traverse_ (LBS8.putStrLn . encode) $ do
--   u1 <- parseBaseUrl "api.example.com"
--   u2 <- parseBaseUrl "example.com"
--   return $ Map.fromList [(u1, 'x'), (u2, 'y')]
-- :}
-- {"http://api.example.com":"x","http://example.com":"y"}
instance ToJSONKey BaseUrl where
    toJSONKey = contramapToJSONKeyFunction showBaseUrl toJSONKey

instance FromJSONKey BaseUrl where
    fromJSONKey = FromJSONKeyTextParser $ \t -> case parseBaseUrl (T.unpack t) of
        Just u  -> return u
        Nothing -> fail $ "Invalid base url: " ++ T.unpack t

-- | >>> showBaseUrl <$> parseBaseUrl "api.example.com"
-- "http://api.example.com"
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

newtype InvalidBaseUrlException = InvalidBaseUrlException String deriving (Show)
instance Exception InvalidBaseUrlException

-- |
--
-- >>> parseBaseUrl "api.example.com"
-- BaseUrl {baseUrlScheme = Http, baseUrlHost = "api.example.com", baseUrlPort = 80, baseUrlPath = ""}
--
-- /Note:/ trailing slash is removed
--
-- >>> parseBaseUrl "api.example.com/"
-- BaseUrl {baseUrlScheme = Http, baseUrlHost = "api.example.com", baseUrlPort = 80, baseUrlPath = ""}
--
-- >>> parseBaseUrl "api.example.com/dir/"
-- BaseUrl {baseUrlScheme = Http, baseUrlHost = "api.example.com", baseUrlPort = 80, baseUrlPath = "/dir"}
--
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

-- $setup
--
-- >>> import Data.Aeson
-- >>> import Data.Foldable (traverse_)
-- >>> import qualified Data.ByteString.Lazy.Char8 as LBS8
-- >>> import qualified Data.Map.Strict as Map
