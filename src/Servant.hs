module Servant (
  -- | This module and its submodules can be used to define servant APIs. Note
  -- that these API definitions don't directly implement a server (or anything
  -- else).
  module Servant.API,
  -- | For implementing servers for servant APIs.
  module Servant.Server,
  -- | For accessing servant APIs as API clients.
  module Servant.Client,
  module Servant.Common.BaseUrl,
  -- | For generating documentation for servant APIs.
  module Servant.Docs,
  -- | Helper module
  module Servant.Common.Text,
  -- | Useful re-exports
  Proxy(..),
  ) where

import Data.Proxy
import Servant.API
import Servant.Client
import Servant.Common.BaseUrl
import Servant.Common.Text
import Servant.Docs
import Servant.Server
