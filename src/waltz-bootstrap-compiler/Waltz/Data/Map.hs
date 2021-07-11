module Waltz.Data.Map (Map, module Waltz.Data.OrderedHashMap) where

import Waltz.Data.OrderedHashMap hiding (OrderedHashMap)
import qualified Waltz.Data.OrderedHashMap

type Map = Waltz.Data.OrderedHashMap.OrderedHashMap
