{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Alternative ((:<|>)(..)) where

#if !MIN_VERSION_base(4,8,0)
import           Data.Monoid   (Monoid (..))
#endif
import           Data.Typeable (Typeable)
-- | Union of two APIs, first takes precedence in case of overlap.
--
-- Example:
--
-- >>> :{
--type MyApi = "books" :> Get '[JSON] [Book] -- GET /books
--        :<|> "books" :> ReqBody '[JSON] Book :> Post '[JSON] () -- POST /books
-- :}
data a :<|> b = a :<|> b
    deriving (Typeable, Eq, Show)
infixr 8 :<|>

instance (Monoid a, Monoid b) => Monoid (a :<|> b) where
    mempty = mempty :<|> mempty
    (a :<|> b) `mappend` (a' :<|> b') = (a `mappend` a') :<|> (b `mappend` b')

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
