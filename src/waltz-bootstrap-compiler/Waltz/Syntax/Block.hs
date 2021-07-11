module Waltz.Syntax.Block where

import Waltz.Prelude
import Text.Megaparsec
import qualified Waltz.Ast
import qualified Waltz.Ast.Syntax
import qualified Waltz.Syntax.Analyzer
import qualified Waltz.Syntax.Common
import qualified Waltz.Syntax.Discard
import qualified Waltz.Syntax.Statement.IfStatement
import qualified Waltz.Syntax.Statement.LetStatement
import qualified Waltz.Syntax.Statement.WithStatement

analyzer :: Waltz.Syntax.Analyzer.Analyzer (Waltz.Ast.Block "syntax-analyzed")
analyzer = do
    Waltz.Syntax.Common.between
        do single '{'
        do single '}'
        do choice
            [ inject <$> Waltz.Syntax.Discard.analyzer "}"
            , inject <$> Waltz.Syntax.Statement.IfStatement.analyzer "}"
            , inject <$> Waltz.Syntax.Statement.LetStatement.analyzer "}"
            , inject <$> Waltz.Syntax.Statement.WithStatement.analyzer "}"
            ]
        do choice
            [ Waltz.Syntax.Common.comment
            , Waltz.Syntax.Common.separator
            , Waltz.Syntax.Common.unexpectedStatement
            , Waltz.Syntax.Common.whitespace
            ]
