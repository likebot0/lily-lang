module Waltz.Config.Model where

import Waltz.Prelude
import Data.Extensible

data T = T
    { entrypoints :: [String]
    , optDirs :: [String]
    , outDir :: String
    , srcDirs :: [String]
    }
