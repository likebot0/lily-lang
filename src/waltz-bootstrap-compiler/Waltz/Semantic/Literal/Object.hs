module Waltz.Semantic.Literal.Object where

import Waltz.Prelude
import qualified Waltz.Ast
import qualified Waltz.Ast.Semantic
import qualified Waltz.Ast.Syntax
import qualified Waltz.Data.Map
import qualified Waltz.Semantic.Analyzer
import qualified Waltz.Semantic.Common
import qualified Waltz.Semantic.Discard
import qualified Waltz.Semantic.KeyValue
import qualified Waltz.Semantic.Statement.IfStatement
import qualified Waltz.Semantic.Statement.IncludeStatement
import qualified Waltz.Semantic.Statement.LetStatement
import qualified Waltz.Semantic.Statement.WithStatement

analyze :: Waltz.Semantic.Analyzer.Analyze "literal/object"
analyze x =
    Waltz.Semantic.Common.newScope \memberStoreRef -> do
    Waltz.Semantic.Common.newScope \variableStoreRef -> do
        let Waltz.Ast.Node children (Waltz.Ast.Syntax.Attributes location) = x

        Waltz.Ast.Node
            <$> (`mapM` children) (
                    do \(x :: Waltz.Ast.Node "syntax-analyzed" "discard") ->
                        inject <$> Waltz.Semantic.Discard.analyze x
                    @>
                    do \(x :: Waltz.Ast.Node "syntax-analyzed" "key-value") ->
                        inject <$> Waltz.Semantic.KeyValue.analyze memberStoreRef x
                    @>
                    do \(x :: Waltz.Ast.Node "syntax-analyzed" "statement/if") ->
                        inject <$> Waltz.Semantic.Statement.IfStatement.analyze x
                    @>
                    do \(x :: Waltz.Ast.Node "syntax-analyzed" "statement/include") ->
                        inject <$> Waltz.Semantic.Statement.IncludeStatement.analyze x
                    @>
                    do \(x :: Waltz.Ast.Node "syntax-analyzed" "statement/let") ->
                        inject <$> Waltz.Semantic.Statement.LetStatement.analyze variableStoreRef x
                    @>
                    do \(x :: Waltz.Ast.Node "syntax-analyzed" "statement/with") ->
                        inject <$> Waltz.Semantic.Statement.WithStatement.analyze x
                    @>
                    typesExhausted
                )
            <*> do
                Waltz.Ast.Semantic.AttributesWithType location
                    <$> Waltz.Ast.Semantic.Type
                        <$> Waltz.Data.Map.map
                            (\(Waltz.Ast.Node _ (Waltz.Ast.Semantic.AttributesWithType _ x)) -> x)
                            <$> get memberStoreRef
