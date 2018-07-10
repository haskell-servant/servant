{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | TODO: or maybe just Servant.SuperRecord?
--
-- TODO: Headers?
module Servant.Server.SuperRecord () where

import           Servant.Multipart
                 (HasMultipartOptions (..), MultipartOptions)
import           Servant.Server ()
import           Servant.Server.Internal
                 (GetNamedContext (..))

import           SuperRecord

-- TODO: hardcoded, there doesn't seem to be machinery to access fields by type in superrecord
instance Has "upload" lts (MultipartOptions tag) => HasMultipartOptions (Rec lts) tag where
    getMultipartOptions = get (FldProxy :: FldProxy "upload")

instance Has l lts v => GetNamedContext (Rec lts) l v where
    getNamedContext _ = get (FldProxy :: FldProxy l)
