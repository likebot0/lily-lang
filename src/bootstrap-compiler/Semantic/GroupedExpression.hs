module Semantic.GroupedExpression where

import Global
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import qualified Semantic.Analyzer
import {-# SOURCE #-} qualified Semantic.Expression

analyze :: Semantic.Analyzer.Analyze "grouped-expression"
analyze x = do
    Ast.Node
        <$> do Semantic.Expression.analyze $ Ast.children x
        <*> do pure $ Ast.attributes x
