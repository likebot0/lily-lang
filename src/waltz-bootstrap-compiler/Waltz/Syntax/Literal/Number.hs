module Waltz.Syntax.Literal.Number where

import Waltz.Prelude
import Text.Megaparsec
import qualified Waltz.Ast
import qualified Waltz.Ast.Syntax
import qualified Waltz.Syntax.Analyzer
import qualified Waltz.Syntax.Common

analyzer :: Waltz.Syntax.Analyzer.Analyzer (Waltz.Ast.Node "syntax-analyzed" "literal/number")
analyzer = Waltz.Syntax.Common.node do
    choice
        [ (:)
            <$> oneOf [ '1', '2', '3', '4', '5', '6', '7', '8', '9' ]
            <*> rest "0123456789"
        , do
            single '0'
            choice
                [ do
                    single 'b'
                    rest "01"
                , do
                    single 'o'
                    rest "01234567"
                , do
                    single 'x'
                    rest "0123456789ABCDEFabcdef"
                , ('0' :) <$>
                    rest "0123456789"
                , pure "0"
                ]
        ]

rest :: String -> Waltz.Syntax.Analyzer.Analyzer String
rest xs =
    manyTill
        do oneOf xs
        do choice
            [ eof
            , () <$ do
                lookAhead $ oneOf Waltz.Syntax.Common.special
            , () <$ do
                lookAhead $ noneOf xs
                Waltz.Syntax.Common.unexpectedToken
            ]
