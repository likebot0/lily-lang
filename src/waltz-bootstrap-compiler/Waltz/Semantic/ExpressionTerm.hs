module Waltz.Semantic.ExpressionTerm where

import Waltz.Prelude
import qualified Waltz.Ast
import qualified Waltz.Ast.Semantic
import qualified Waltz.Ast.Syntax
import qualified Waltz.Semantic.Analyzer
import qualified Waltz.Semantic.BlockExpression
import qualified Waltz.Semantic.GroupedExpression
import qualified Waltz.Semantic.Identifier
import qualified Waltz.Semantic.LambdaExpression
import qualified Waltz.Semantic.Literal.Array
import qualified Waltz.Semantic.Literal.Number
import qualified Waltz.Semantic.Literal.Object
import qualified Waltz.Semantic.Literal.String
import qualified Waltz.Semantic.TypeExpression

analyze
    :: forall e. (LastMember IO e, Members Waltz.Semantic.Analyzer.Effects e)
    => Union (Waltz.Ast.ExpressionTerm "syntax-analyzed")
    -> Eff e (Union (Waltz.Ast.ExpressionTerm "semantic-analyzed"))
analyze =
    do \(x :: Waltz.Ast.Node "syntax-analyzed" "block-expression") -> do
        inject <$> Waltz.Semantic.BlockExpression.analyze x
    @>
    do \(x :: Waltz.Ast.Node "syntax-analyzed" "grouped-expression") -> do
        inject <$> Waltz.Semantic.GroupedExpression.analyze x
    @>
    do \(x :: Waltz.Ast.Node "syntax-analyzed" "identifier") -> do
        inject <$> Waltz.Semantic.Identifier.analyze x
    @>
    do \(x :: Waltz.Ast.Node "syntax-analyzed" "lambda-expression") -> do
        inject <$> Waltz.Semantic.LambdaExpression.analyze x
    @>
    do \(x :: Waltz.Ast.Node "syntax-analyzed" "literal/array") -> do
        inject <$> Waltz.Semantic.Literal.Array.analyze x
    @>
    do \(x :: Waltz.Ast.Node "syntax-analyzed" "literal/number") -> do
        inject <$> Waltz.Semantic.Literal.Number.analyze x
    @>
    do \(x :: Waltz.Ast.Node "syntax-analyzed" "literal/object") -> do
        inject <$> Waltz.Semantic.Literal.Object.analyze x
    @>
    do \(x :: Waltz.Ast.Node "syntax-analyzed" "literal/string") -> do
        inject <$> Waltz.Semantic.Literal.String.analyze x
    @>
    do \(x :: Waltz.Ast.Node "syntax-analyzed" "type-expression") -> do
        inject <$> Waltz.Semantic.TypeExpression.analyze x
    @>
    typesExhausted
