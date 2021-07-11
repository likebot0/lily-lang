module Waltz.Semantic.BlockExpression where

import Waltz.Prelude
import qualified Waltz.Ast
import qualified Waltz.Ast.Semantic
import qualified Waltz.Ast.Syntax
import qualified Waltz.Semantic.Analyzer
import qualified Waltz.Semantic.Block

analyze :: Waltz.Semantic.Analyzer.Analyze "block-expression"
analyze x = do
    let Waltz.Ast.Node children (Waltz.Ast.Syntax.Attributes location) = x

    Waltz.Ast.Node
        <$> do Waltz.Semantic.Block.analyze $ Waltz.Ast.children x
        <*> do pure $ Waltz.Ast.Semantic.AttributesWithType location $ Waltz.Ast.Semantic.Type ()

    |> withOverride @"waltz-bootstrap-compiler/context/resolve" do
        \super -> fun \identifier -> do
            if identifier == "break"
                then return $ inject Waltz.Semantic.Analyzer.mock
                else pure ()

            super identifier
