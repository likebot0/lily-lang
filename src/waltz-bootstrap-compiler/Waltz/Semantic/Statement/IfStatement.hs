module Waltz.Semantic.Statement.IfStatement where

import Waltz.Prelude
import qualified Waltz.Ast
import qualified Waltz.Ast.Semantic
import qualified Waltz.Ast.Syntax
import qualified Waltz.Semantic.Analyzer
import {-# SOURCE #-} qualified Waltz.Semantic.Block
import qualified Waltz.Semantic.Expression

analyze :: Waltz.Semantic.Analyzer.Analyze "statement/if"
analyze x = do
    let (condition, body) = Waltz.Ast.children x

    Waltz.Ast.Node
        <$> ((,)
            <$> Waltz.Semantic.Expression.analyze condition
            <*> Waltz.Semantic.Block.analyze body
        )
        <*> do pure $ Waltz.Ast.attributes x
