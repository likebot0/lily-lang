module Waltz.Syntax.Statement.WithStatement where

import Waltz.Prelude
import Text.Megaparsec
import qualified Waltz.Ast
import qualified Waltz.Ast.Syntax
import qualified Waltz.Syntax.Analyzer
import qualified Waltz.Syntax.Common
import qualified Waltz.Syntax.Expression

analyzer :: String -> Waltz.Syntax.Analyzer.Analyzer (Waltz.Ast.Node "syntax-analyzed" "statement/with")
analyzer end = Waltz.Syntax.Common.node do
    Waltz.Syntax.Common.keyword "\\with"

    Just expression <- Waltz.Syntax.Common.expect
        do Waltz.Syntax.Expression.analyzer end

    Waltz.Syntax.Common.expect
        do choice
            [ eof
            , () <$ do lookAhead $ oneOf $ [ '\r', '\n', '#', ',' ]  ++ end
            ]

    pure expression
