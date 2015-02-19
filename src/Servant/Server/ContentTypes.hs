{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.Server.ContentTypes where

import Control.Monad (join)
import Control.Arrow (left)
import Data.Aeson (ToJSON(..), FromJSON(..), encode, eitherDecode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as BS
import Data.Proxy (Proxy(..))
import Data.String.Conversions (cs)
import qualified Data.Text.Lazy.Encoding as Text
import qualified Data.Text.Lazy as Text
import GHC.Exts (Constraint)
import qualified Network.HTTP.Media as M


import Servant.API ( XML, HTML, JSON, JavaScript, CSS, PlainText
                   , OctetStream, MimeRender(..), MimeUnrender(..) )


--------------------------------------------------------------------------
-- * MimeRender Instances

-- | @encode@
instance ToJSON a => MimeRender JSON a where
    toByteString _ = encode

-- | @encodeUtf8@
instance MimeRender PlainText Text.Text where
    toByteString _ = Text.encodeUtf8

--------------------------------------------------------------------------
-- * MimeUnrender Instances

-- | @decode@
instance FromJSON a => MimeUnrender JSON a where
    fromByteString _ = eitherDecode

-- | @Text.decodeUtf8'@
instance MimeUnrender PlainText Text.Text where
    fromByteString _ = left show . Text.decodeUtf8'

