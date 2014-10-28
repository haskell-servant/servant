module Servant (
  -- | This module and its submodules can be used to define servant APIs. Note
  -- that these API definitions don't directly implement a server (or anything
  -- else).
  module Servant.API,
  -- | For implementing servers for servant APIs.
  module Servant.Server,
  -- | For accessing servant APIs as API clients.
  module Servant.Client,
  -- | Helper module
  module Servant.Text,
  ) where

import Servant.API
import Servant.Client
import Servant.Server
import Servant.Text
