{-# LANGUAGE PackageImports #-}
module HLint.HLint where

import           "hint" HLint.Builtin.All
import           "hint" HLint.Default
import           "hint" HLint.Dollar
import           "hint" HLint.Generalise

ignore "Redundant bracket" = Domains.Rest.Client
ignore "Use mappend"
