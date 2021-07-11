module Waltz.Semantic.GroupedExpression where

import Waltz.Prelude
import Waltz.Declare
import qualified Waltz.Ast
import qualified Waltz.Ast.Semantic
import qualified Waltz.Ast.Syntax
import qualified Waltz.Semantic.Analyzer
import qualified Waltz.Semantic.Common
import {-# SOURCE #-} qualified Waltz.Semantic.ExpressionTerm

analyze :: Waltz.Semantic.Analyzer.Analyze "grouped-expression"
analyze x = do
    let Waltz.Ast.Node children (Waltz.Ast.Syntax.Attributes location) = x

    termRef <- new Nothing

    Waltz.Ast.Node
        <$> (`mapM` children) (\child -> do
            ast <- Waltz.Semantic.ExpressionTerm.analyze child

            let (l, t) = Waltz.Semantic.Common.loc ast

            result <- get termRef >>= \case
                Nothing -> pure t
                Just f -> Waltz.Semantic.Common.apply f t l

            set termRef $ Just result

            pure ast
        )
        <*> do Waltz.Ast.Semantic.AttributesWithType
                do location
                <$> (`fmap` get termRef) \case
                    Nothing -> Waltz.Ast.Semantic.Type $ inject $ Waltz.Ast.Semantic.Type @"unknown" ()
                    Just x -> Waltz.Ast.Semantic.Type x
