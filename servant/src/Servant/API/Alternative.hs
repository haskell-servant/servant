{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators      #-}
module Servant.API.Alternative ((:<|>)(..)) where

import           Data.Monoid   (Monoid (..))
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
