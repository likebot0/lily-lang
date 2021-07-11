module Waltz.Syntax.ExpressionTerm where

import Waltz.Prelude
import qualified Waltz.Ast
import qualified Waltz.Ast.Syntax
import qualified Waltz.Syntax.Analyzer

analyzer :: Waltz.Syntax.Analyzer.Analyzer (Union (Waltz.Ast.ExpressionTerm "syntax-analyzed"))
