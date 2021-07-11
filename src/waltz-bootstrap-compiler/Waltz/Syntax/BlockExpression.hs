module Waltz.Syntax.BlockExpression where

import Waltz.Prelude
import Text.Megaparsec
import qualified Waltz.Ast
import qualified Waltz.Ast.Syntax
import qualified Waltz.Syntax.Analyzer
import qualified Waltz.Syntax.Block
import qualified Waltz.Syntax.Common

analyzer :: Waltz.Syntax.Analyzer.Analyzer (Waltz.Ast.Node "syntax-analyzed" "block-expression")
analyzer = Waltz.Syntax.Common.node do
    choice
        [ Waltz.Syntax.Common.keyword "do"
        , Waltz.Syntax.Common.keyword "repeat"
        , Waltz.Syntax.Common.keyword "static-do"
        , Waltz.Syntax.Common.keyword "static-repeat"
        ]

    Just x <- Waltz.Syntax.Common.expect
        do Waltz.Syntax.Block.analyzer

    pure x
