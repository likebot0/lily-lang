module Waltz.Syntax.Root where

import Waltz.Prelude
import Text.Megaparsec
import qualified Waltz.Ast
import qualified Waltz.Ast.Syntax
import qualified Waltz.Syntax.Analyzer
import qualified Waltz.Syntax.Common
import qualified Waltz.Syntax.Expression
import qualified Waltz.Syntax.Statement.BaseStatement

analyzer :: Waltz.Syntax.Analyzer.Analyzer (Waltz.Ast.Node "syntax-analyzed" "root")
analyzer = Waltz.Syntax.Common.node do
    Waltz.Syntax.Common.manyTill
        do choice
            [ inject <$> Waltz.Syntax.Expression.analyzer ","
            , inject <$> Waltz.Syntax.Statement.BaseStatement.analyzer
            ]
        do eof
        do choice
            [ Waltz.Syntax.Common.comment
            , Waltz.Syntax.Common.separator
            , Waltz.Syntax.Common.unexpectedStatement
            , Waltz.Syntax.Common.whitespace
            ]
