module Semantic.Statement.LetStatement where

import Global
import qualified Ast
import qualified Ast.Semantic
import qualified Ast.Syntax
import qualified Data.HashMap.Strict
import qualified Semantic.Analyzer
import qualified Semantic.KeyValue

analyze :: IORef (Data.HashMap.Strict.HashMap String (Ast.Node "semantic-analyzed" "expression")) -> Semantic.Analyzer.Analyze "statement/let"
analyze memberStoreRef x = do
    keyValueNode <- Semantic.KeyValue.analyze $ Ast.children x

    let (key, value) = Ast.children keyValueNode

    memberStore <- get memberStoreRef

    set memberStoreRef $ Data.HashMap.Strict.insert (Ast.children key) value memberStore

    pure $ Ast.Node
        do keyValueNode
        do Ast.attributes x
