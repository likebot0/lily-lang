module Waltz.Syntax.TypeExpression where

import Waltz.Prelude
import Data.List
import Text.Megaparsec
import qualified Waltz.Ast
import qualified Waltz.Ast.Syntax
import qualified Waltz.Syntax.Analyzer
import qualified Waltz.Syntax.Common
import {-# SOURCE #-} qualified Waltz.Syntax.ExpressionTerm

analyzer :: Waltz.Syntax.Analyzer.Analyzer (Waltz.Ast.Node "syntax-analyzed" "type-expression")
analyzer = Waltz.Syntax.Common.node do
    Waltz.Syntax.Common.keyword "type"

    Just x <- Waltz.Syntax.Common.expect
        do Waltz.Syntax.ExpressionTerm.analyzer

    pure x
