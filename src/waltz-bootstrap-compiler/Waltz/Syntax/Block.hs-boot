module Waltz.Syntax.Block where

import Waltz.Prelude
import qualified Waltz.Ast
import qualified Waltz.Ast.Syntax
import qualified Waltz.Syntax.Analyzer

analyzer :: Waltz.Syntax.Analyzer.Analyzer (Waltz.Ast.Block "syntax-analyzed")
