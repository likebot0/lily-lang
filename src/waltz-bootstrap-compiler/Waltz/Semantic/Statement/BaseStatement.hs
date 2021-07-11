module Waltz.Semantic.Statement.BaseStatement where

import Waltz.Prelude
import qualified Waltz.Ast
import qualified Waltz.Ast.Semantic
import qualified Waltz.Ast.Syntax
import qualified Waltz.Semantic.Analyzer
import qualified Waltz.Semantic.Identifier

analyze :: Waltz.Semantic.Analyzer.Analyze "statement/base"
analyze x = do
    Waltz.Ast.Node
        <$> do Waltz.Semantic.Identifier.analyze $ Waltz.Ast.children x
        <*> do pure $ Waltz.Ast.attributes x
