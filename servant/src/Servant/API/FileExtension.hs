{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Servant.API.FileExtension
    ( Ext(..)
    , getExt
    , parseExt
    , renderExt
    , toExtProxy
    ) where

import           Control.Monad   ((>=>))
import           Data.Maybe      (catMaybes)
import           Data.Proxy
import           Data.Text       (Text, pack, stripSuffix)
import           Data.Typeable   (Typeable)
import           GHC.TypeLits
import           Web.HttpApiData
#if !MIN_VERSION_base(4,8,0)
import           Data.Monoid     (mappend)
#endif

-- | A wrapper around a `Text` value which must be suffixed by the extension `ext`.
--
-- Pattern matching on the `Ext` constructor will give you the `Text` value without
-- the suffix, use `renderExt` to retrive the full string including '.' and extension.
--
-- Example:
--
-- >>>            -- GET /file/:filename.png
-- >>> type MyApi = "file" :> Capture "filename" (Ext "png") :> Get '[JSON] Text
--
-- >>> x :: Ext "png"
-- >>> x = Ext "mypic"
-- >>> renderExt x
-- "mypic.png"
--
newtype Ext (ext :: Symbol) = Ext {getFileName :: Text}
    deriving (Typeable, Eq, Ord)

instance (KnownSymbol ext) => Show (Ext ext) where
    showsPrec i tt@(Ext t) = showsPrec i (mappend t . pack $ '.':getExt tt)

instance (KnownSymbol ext) => Read (Ext ext) where
    readsPrec i str = res
        where
            -- res :: [(Ext ext t, String)]
            res = catMaybes . map f . readsPrec (i+1) $ str

            f :: (Text,String) -> Maybe (Ext ext,String)
            f (t,s) = fmap (\txt -> (Ext txt,s)) $ stripSuffix (pack $ '.':ext) t

            toExtTy :: [(Ext ext, String)] -> Ext ext
            toExtTy _ = undefined

            ext = getExt (toExtTy res)



instance (KnownSymbol ext) => ToHttpApiData (Ext ext) where
    toUrlPiece   = toUrlPiece   . renderExt
    toHeader     = toHeader     . renderExt
    toQueryParam = toQueryParam . renderExt


instance (KnownSymbol ext) => FromHttpApiData (Ext ext) where
    parseUrlPiece   = parseUrlPiece   >=> parseExt
    parseHeader     = parseHeader     >=> parseExt
    parseQueryParam = parseQueryParam >=> parseExt


toExtProxy :: Ext ext-> Proxy ext
toExtProxy _ = Proxy

getExt :: KnownSymbol ext => Ext ext -> String
getExt t = symbolVal (toExtProxy t)

parseExt :: (KnownSymbol ext) => String -> Either Text (Ext ext)
parseExt str = res
    where
        res = case stripSuffix (pack ext) (pack str) of
                Nothing -> Left . pack $ "The filename \"" ++ str ++ "\" does not end with extension \"" ++ ext ++ "\""
                Just txt -> Right (Ext txt)

        ext = '.':getExt (toExtTy res)

        toExtTy :: Either Text (Ext ext) -> Ext ext
        toExtTy _ = undefined

renderExt :: KnownSymbol ext => Ext ext -> Text
renderExt ee@(Ext t) = mappend t (pack $ '.':getExt ee)


-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text

