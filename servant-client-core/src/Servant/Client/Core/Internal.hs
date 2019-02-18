module Servant.Client.Core.Internal where

import           Control.DeepSeq
                 (rnf)
import           Network.HTTP.Media
                 (MediaType, mainType, parameters, subType)

mediaTypeRnf :: MediaType -> ()
mediaTypeRnf mt =
    rnf (mainType mt) `seq`
    rnf (subType mt) `seq`
    rnf (parameters mt)
