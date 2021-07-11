module Waltz.Semantic.Block where

import Waltz.Prelude
import qualified Data.HashMap.Strict
import qualified Waltz.Ast
import qualified Waltz.Ast.Semantic
import qualified Waltz.Ast.Syntax
import qualified Waltz.Semantic.Analyzer
import qualified Waltz.Semantic.Common
import qualified Waltz.Semantic.Discard
import qualified Waltz.Semantic.KeyValue
import qualified Waltz.Semantic.Statement.IfStatement
import qualified Waltz.Semantic.Statement.LetStatement
import qualified Waltz.Semantic.Statement.WithStatement

analyze :: (LastMember IO e, Members Waltz.Semantic.Analyzer.Effects e) =>
    Waltz.Ast.Block "syntax-analyzed" -> Eff e (Waltz.Ast.Block "semantic-analyzed")
analyze body = do
    Waltz.Semantic.Common.newScope \variableStoreRef ->
        (`mapM` body) (
            do \(x :: Waltz.Ast.Node "syntax-analyzed" "discard") ->
                inject <$> Waltz.Semantic.Discard.analyze x
            @>
            do \(x :: Waltz.Ast.Node "syntax-analyzed" "statement/if") ->
                inject <$> Waltz.Semantic.Statement.IfStatement.analyze x
            @>
            do \(x :: Waltz.Ast.Node "syntax-analyzed" "statement/let") ->
                inject <$> Waltz.Semantic.Statement.LetStatement.analyze variableStoreRef x
            @>
            do \(x :: Waltz.Ast.Node "syntax-analyzed" "statement/with") ->
                inject <$> Waltz.Semantic.Statement.WithStatement.analyze x
            @>
            typesExhausted
        )
