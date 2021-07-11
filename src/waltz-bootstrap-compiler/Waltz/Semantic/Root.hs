module Waltz.Semantic.Root where

import Waltz.Prelude
import qualified Waltz.Ast
import qualified Waltz.Ast.Semantic
import qualified Waltz.Ast.Syntax
import qualified Waltz.Semantic.Analyzer
import qualified Waltz.Semantic.Expression
import qualified Waltz.Semantic.Statement.BaseStatement

analyze :: Waltz.Semantic.Analyzer.Analyze "root"
analyze x = do
    Waltz.Ast.Node
        <$> (`mapM` Waltz.Ast.children x) (
                do \(x :: Waltz.Ast.Node "syntax-analyzed" "expression") ->
                    inject <$> Waltz.Semantic.Expression.analyze x
                @>
                do \(x :: Waltz.Ast.Node "syntax-analyzed" "statement/base") ->
                    inject <$> Waltz.Semantic.Statement.BaseStatement.analyze x
                @>
                typesExhausted
            )
        <*> do pure $ Waltz.Ast.attributes x
