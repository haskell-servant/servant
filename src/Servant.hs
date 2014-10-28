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
  -- | Helper module
  module Servant.Utils.Text,
  ) where

import Servant.API
import Servant.Client
import Servant.Docs
import Servant.Server
import Servant.Utils.Text
