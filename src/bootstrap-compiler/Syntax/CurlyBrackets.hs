module Syntax.CurlyBrackets where

import Global
import Text.Megaparsec
import qualified Ast
import qualified Ast.Syntax
import qualified Syntax.Analyzer
import qualified Syntax.Comment
import qualified Syntax.Discard
import qualified Syntax.Separator
import qualified Syntax.Common
import qualified Syntax.Statement.IfStatement
import qualified Syntax.Statement.LetStatement
import qualified Syntax.Statement.WithStatement
import qualified Syntax.UnexpectedStatement
import qualified Syntax.Whitespace

analyzer :: Syntax.Analyzer.Analyzer (Ast.CurlyBracketsBody "syntax-analyzed")
analyzer =
    Syntax.Common.between
        do single '{'
        do single '}'
        do choice
            [ inject <$> Syntax.Discard.analyzer "}"
            , inject <$> Syntax.Statement.IfStatement.analyzer "}"
            , inject <$> Syntax.Statement.LetStatement.analyzer "}"
            , inject <$> Syntax.Statement.WithStatement.analyzer "}"
            ]
        do choice
            [ Syntax.Comment.analyzer
            , Syntax.Separator.analyzer
            , Syntax.UnexpectedStatement.analyzer
            , Syntax.Whitespace.analyzer
            ]
