module Waltz.Semantic.TypeExpression where

import Waltz.Prelude
import Waltz.Declare
import qualified Waltz.Ast
import qualified Waltz.Ast.Semantic
import qualified Waltz.Ast.Syntax
import qualified Waltz.Semantic.Analyzer
import qualified Waltz.Semantic.Common
import {-# SOURCE #-} qualified Waltz.Semantic.ExpressionTerm

analyze :: Waltz.Semantic.Analyzer.Analyze "type-expression"
analyze x = do
    let Waltz.Ast.Node children (Waltz.Ast.Syntax.Attributes location) = x

    Waltz.Ast.Node
        <$> Waltz.Semantic.ExpressionTerm.analyze children
        <*> do pure $ Waltz.Ast.Semantic.AttributesWithType location $ Waltz.Ast.Semantic.Type ()
