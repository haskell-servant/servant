module Servant (
  -- | This module and its submodules can be used to define servant APIs. Note
  -- that these API definitions don't directly implement a server (or anything
  -- else).
  module Servant.API,
  -- | For implementing servers for servant APIs.
  module Servant.Server,
  -- | Utilities on top of the servant core
  module Servant.Links,
  module Servant.Utils.StaticFiles,
  -- | Useful re-exports
  Proxy(..),
  throwError
  ) where

import           Control.Monad.Error.Class (throwError)
import           Data.Proxy
import           Servant.API
import           Servant.Server
import           Servant.Links
import           Servant.Utils.StaticFiles
