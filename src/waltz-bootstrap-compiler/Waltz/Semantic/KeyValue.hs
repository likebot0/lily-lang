module Waltz.Semantic.KeyValue where

import Waltz.Prelude
import qualified Waltz.Ast
import qualified Waltz.Ast.Semantic
import qualified Waltz.Ast.Syntax
import qualified Waltz.Data.Map
import qualified Waltz.Semantic.Analyzer
import qualified Waltz.Semantic.Expression

type VariableStore = Waltz.Data.Map.Map String (Waltz.Ast.Node "semantic-analyzed" "expression")

analyze :: IORef VariableStore -> Waltz.Semantic.Analyzer.Analyze "key-value"
analyze variableStoreRef x = do
    let (key, value) = Waltz.Ast.children x

    value <- Waltz.Semantic.Expression.analyze value

    variableStore <- get variableStoreRef

    set variableStoreRef $ Waltz.Data.Map.insert key value variableStore

    pure $ Waltz.Ast.Node (key, value) $ Waltz.Ast.attributes x
