module Servant (
  -- | This module and its submodules can be used to define servant APIs. Note
  -- that these API definitions don't directly implement a server (or anything
  -- else).
  module Servant.API,
  -- | For implementing servers for servant APIs.
  module Servant.Server,
  -- | For accessing servant APIs as API clients.
  module Servant.Client,
  -- | For generating documentation for servant APIs.
  module Servant.Docs,
  -- | Using your types in request paths and query string parameters
  module Servant.Common.Text,
  -- | Utilities on top of the servant core
  module Servant.QQ,
  module Servant.Utils.Links,
  module Servant.Utils.StaticFiles,
  -- | Useful re-exports
  Proxy(..),
  ) where

import Data.Proxy
import Servant.API
import Servant.Client
import Servant.Common.Text
import Servant.Docs
import Servant.Server
import Servant.QQ
import Servant.Utils.Links
import Servant.Utils.StaticFiles
