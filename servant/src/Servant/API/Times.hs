{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Servant.API.Times where

import           Data.Typeable (Typeable)
import           GHC.TypeLits  (Symbol)


-- | Capture data/time value from a path in the `format', as understood
-- by 'Data.Time.Format.
--
-- Example:
-- >>>            -- GET /events/:date
-- >>> type MyApi = "events" :> CaptureTime "date" "%Y-%m-%d" Day :> Get '[JSON] Book
data CaptureTime (sym :: Symbol) (format :: Symbol) a
    deriving (Typeable)


data QueryParamTime (sym :: Symbol) (format :: Symbol) a
    deriving (Typeable)


data QueryParamTimes (sym :: Symbol) (format :: Symbol) a
    deriving (Typeable)

-- $setup
-- >>> import Servant.API
-- >>> import Data.Aeson
-- >>> import Data.Text
-- >>> import Data.Time.Calendar
-- >>> data Book
-- >>> instance ToJSON Book where { toJSON = undefined }
