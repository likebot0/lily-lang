module Waltz.Syntax.Literal.String where

import Waltz.Prelude
import Text.Megaparsec
import qualified Waltz.Ast
import qualified Waltz.Ast.Syntax
import qualified Waltz.Syntax.Analyzer
import qualified Waltz.Syntax.Common
import qualified Waltz.Syntax.GroupedExpression

analyzer :: Waltz.Syntax.Analyzer.Analyzer (Waltz.Ast.Node "syntax-analyzed" "literal/string")
analyzer = Waltz.Syntax.Common.node do
    Waltz.Syntax.Common.between
        do single '"'
        do single '"'
        do choice
            [ inject <$> some 
                do choice
                    [ noneOf [ '"', '\\' ]
                    , charEscape
                    ]
            , inject <$> interpolation
            ]
        do
            single '\\'
            choice
                [ Waltz.Syntax.Common.unexpectedEndOfInput
                , Waltz.Syntax.Common.unexpectedToken
                ]

charEscape :: Waltz.Syntax.Analyzer.Analyzer Char
charEscape = try do
    single '\\'

    choice
        [ '\\' <$ single '\\'
        , '\"' <$ single '"'
        , '\t' <$ single 't'
        , '\r' <$ single 'r'
        , '\n' <$ single 'n'
        ]

interpolation :: Waltz.Syntax.Analyzer.Analyzer (Waltz.Ast.Node "syntax-analyzed" "grouped-expression")
interpolation = try do
    single '\\'

    Waltz.Syntax.GroupedExpression.analyzer
