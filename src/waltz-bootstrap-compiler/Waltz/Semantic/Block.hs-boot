module Waltz.Semantic.Block where

import Waltz.Prelude
import qualified Data.HashMap.Strict
import qualified Waltz.Ast
import qualified Waltz.Semantic.Analyzer

analyze :: (LastMember IO e, Members Waltz.Semantic.Analyzer.Effects e) =>
    Waltz.Ast.Block "syntax-analyzed" -> Eff e (Waltz.Ast.Block "semantic-analyzed")
