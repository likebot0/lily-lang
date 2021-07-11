module Waltz.Semantic.LambdaExpression where

import Waltz.Prelude
import qualified Waltz.Ast
import qualified Waltz.Ast.Semantic
import qualified Waltz.Ast.Syntax
import qualified Waltz.Data.Map
import qualified Waltz.Semantic.Analyzer
import {-# SOURCE #-} qualified Waltz.Semantic.Block

analyze :: Waltz.Semantic.Analyzer.Analyze "lambda-expression"
analyze x = do
    let Waltz.Ast.Node children (Waltz.Ast.Syntax.Attributes location) = x

    let (argument, body) = children

    argumentStoreRef <- new $ Waltz.Data.Map.empty @String @(Waltz.Ast.Node "semantic-analyzed" "expression")

    argumentStore <- get argumentStoreRef

    set argumentStoreRef $ Waltz.Data.Map.insert argument undefined argumentStore

    do
        Waltz.Ast.Node
            <$> ((,)
                argument
                <$> Waltz.Semantic.Block.analyze body
            )
            <*> do pure $ Waltz.Ast.Semantic.AttributesWithType location $ Waltz.Ast.Semantic.Type ()

        |> withOverride @"waltz-bootstrap-compiler/context/resolve-local" do
            \super -> fun \identifier -> do
                argumentStore <- get argumentStoreRef

                Waltz.Data.Map.lookup identifier argumentStore |> \case
                    Just x -> return Waltz.Semantic.Analyzer.mock
                    _ -> pure ()

                super identifier
