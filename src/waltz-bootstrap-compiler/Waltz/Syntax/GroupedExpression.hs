module Waltz.Syntax.GroupedExpression where

import Waltz.Prelude
import Text.Megaparsec
import qualified Waltz.Ast
import qualified Waltz.Ast.Syntax
import qualified Waltz.Syntax.Analyzer
import qualified Waltz.Syntax.Common
import {-# SOURCE #-} qualified Waltz.Syntax.ExpressionTerm

analyzer :: Waltz.Syntax.Analyzer.Analyzer (Waltz.Ast.Node "syntax-analyzed" "grouped-expression")
analyzer = Waltz.Syntax.Common.node do
    single '('

    Just terms <- Waltz.Syntax.Common.expect
        do Waltz.Syntax.Common.someTill
            do Waltz.Syntax.ExpressionTerm.analyzer
            do choice
                [ eof
                , () <$ do lookAhead $ oneOf [ '\r', '\n', '#', ')' ]
                ]
            do choice
                [ Waltz.Syntax.Common.whitespace
                ]

    Waltz.Syntax.Common.expect
        do single ')'

    pure terms 
