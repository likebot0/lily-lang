module Waltz.Syntax.Identifier where

import Waltz.Prelude
import Text.Megaparsec
import qualified Waltz.Ast
import qualified Waltz.Ast.Syntax
import qualified Waltz.Syntax.Analyzer
import qualified Waltz.Syntax.Common

analyzer :: Waltz.Syntax.Analyzer.Analyzer (Waltz.Ast.Node "syntax-analyzed" "identifier")
analyzer = Waltz.Syntax.Common.node do
    some $ noneOf Waltz.Syntax.Common.special
