
module Servant.MockSpec where

import           Test.Hspec

import           Servant.API.Internal.Test.ComprehensiveAPI
import           Servant.Mock

_ = mock comprehensiveAPI

spec :: Spec
spec = return ()
