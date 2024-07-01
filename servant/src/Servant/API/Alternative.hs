{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE CPP      #-}
{-# OPTIONS_HADDOCK not-home    #-}
module Servant.API.Alternative ((:<|>)(..)) where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import           Data.Biapplicative
                 (Biapplicative (..))
import           Data.Bifoldable
                 (Bifoldable (..))
import           Data.Bifunctor
                 (Bifunctor (..))
import           Data.Bitraversable
                 (Bitraversable (..))
import           Data.Typeable
                 (Typeable)

-- | Union of two APIs, first takes precedence in case of overlap.
--
-- Example:
--
-- >>> :{
--type MyApi = "books" :> Get '[JSON] [Book] -- GET /books
--        :<|> "books" :> ReqBody '[JSON] Book :> Post '[JSON] () -- POST /books
-- :}
data a :<|> b = a :<|> b
    deriving (Typeable, Eq, Show, Functor, Traversable, Foldable, Bounded)
infixr 3 :<|>

instance (Semigroup a, Semigroup b) => Semigroup (a :<|> b) where
    (a :<|> b) <> (a' :<|> b') = (a <> a') :<|> (b <> b')

instance (Monoid a, Monoid b) => Monoid (a :<|> b) where
    mempty = mempty :<|> mempty

instance Bifoldable (:<|>) where
    bifoldMap f g ~(a :<|> b) = f a `mappend` g b

instance Bifunctor (:<|>) where
    bimap f g ~(a :<|> b) = f a :<|> g b

instance Biapplicative (:<|>) where
    bipure = (:<|>)
    (f :<|> g) <<*>> (a :<|> b) = f a :<|> g b

instance Bitraversable (:<|>) where
    bitraverse f g ~(a :<|> b) = liftA2 (:<|>) (f a) (g b)

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
