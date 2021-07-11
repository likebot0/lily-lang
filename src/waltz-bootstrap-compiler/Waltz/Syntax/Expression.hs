module Waltz.Syntax.Expression where

import Waltz.Prelude
import Text.Megaparsec
import qualified Waltz.Ast
import qualified Waltz.Ast.Syntax
import qualified Waltz.Syntax.Analyzer
import qualified Waltz.Syntax.Common
import {-# SOURCE #-} qualified Waltz.Syntax.ExpressionTerm

analyzer :: String -> Waltz.Syntax.Analyzer.Analyzer (Waltz.Ast.Node "syntax-analyzed" "expression")
analyzer end = Waltz.Syntax.Common.node do
    Waltz.Syntax.Common.someTill
        do Waltz.Syntax.ExpressionTerm.analyzer
        do choice
            [ eof
            , () <$ do lookAhead $ oneOf $ [ '\r', '\n', '#' ] ++ end
            ]
        do choice
            [ Waltz.Syntax.Common.whitespace
            ]
