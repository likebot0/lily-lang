module Waltz.Syntax.Statement.BaseStatement where

import Waltz.Prelude
import Text.Megaparsec
import qualified Waltz.Ast
import qualified Waltz.Ast.Syntax
import qualified Waltz.Syntax.Analyzer
import qualified Waltz.Syntax.Common
import qualified Waltz.Syntax.Identifier

analyzer :: Waltz.Syntax.Analyzer.Analyzer (Waltz.Ast.Node "syntax-analyzed" "statement/base")
analyzer = Waltz.Syntax.Common.node do
    Waltz.Syntax.Common.keyword "\\base"

    Just identifier <- Waltz.Syntax.Common.expect
        do Waltz.Syntax.Identifier.analyzer

    Waltz.Syntax.Common.expect
        do lookAhead $ oneOf [ '\r', '\n', '#', ',' ]

    pure identifier
