module Waltz.Semantic.ExpressionTerm where

import Waltz.Prelude
import qualified Waltz.Ast
import qualified Waltz.Ast.Semantic
import qualified Waltz.Ast.Syntax
import qualified Waltz.Semantic.Analyzer

analyze
    :: forall e. (LastMember IO e, Members Waltz.Semantic.Analyzer.Effects e)
    => Union (Waltz.Ast.ExpressionTerm "syntax-analyzed")
    -> Eff e (Union (Waltz.Ast.ExpressionTerm "semantic-analyzed"))
