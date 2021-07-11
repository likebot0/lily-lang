module Waltz.Semantic.Literal.String where

import Waltz.Prelude
import qualified Waltz.Ast
import qualified Waltz.Ast.Semantic
import qualified Waltz.Ast.Syntax
import qualified Waltz.Semantic.Analyzer

analyze :: Waltz.Semantic.Analyzer.Analyze "literal/string"
analyze x = do
    let Waltz.Ast.Node children (Waltz.Ast.Syntax.Attributes location) = x

    pure $ Waltz.Ast.Node
        do [ inject ("" :: String) ]
        do Waltz.Ast.Semantic.AttributesWithType
            do location
            do Waltz.Ast.Semantic.Type ""
