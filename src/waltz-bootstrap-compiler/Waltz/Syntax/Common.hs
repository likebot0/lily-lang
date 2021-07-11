module Waltz.Syntax.Common where

import Waltz.Prelude
import Waltz.Declare
import Data.Text
import Text.Megaparsec
import qualified Waltz.Ast
import qualified Waltz.Ast.Syntax
import qualified Waltz.Diagnostic
import qualified Waltz.Syntax.Analyzer

position :: Waltz.Syntax.Analyzer.Analyzer Waltz.Diagnostic.Position
position = do
    x <- getSourcePos

    pure $ Waltz.Diagnostic.Position
        do unPos $ sourceLine x
        do unPos $ sourceColumn x

node
    :: Waltz.Syntax.Analyzer.Analyzer (Waltz.Ast.Children "syntax-analyzed" a)
    -> Waltz.Syntax.Analyzer.Analyzer (Waltz.Ast.Node "syntax-analyzed" a)
node p = do
    start <- position

    Waltz.Ast.Node
        <$> p
        <*> do Waltz.Ast.Syntax.Attributes . Waltz.Diagnostic.Location start <$> position
{-# INLINE node #-}

manyTill
    :: Waltz.Syntax.Analyzer.Analyzer a
    -> Waltz.Syntax.Analyzer.Analyzer end
    -> Waltz.Syntax.Analyzer.Analyzer skip
    -> Waltz.Syntax.Analyzer.Analyzer [a]
manyTill p end skip = do
    go
    where
        go = choice
            [ [] <$ end
            , [] <$ unexpectedEndOfInput
            , (:) <$> p <*> go
            , skip *> go
            , unexpectedToken *> go
            ]
{-# INLINE manyTill #-}

someTill
    :: Waltz.Syntax.Analyzer.Analyzer a
    -> Waltz.Syntax.Analyzer.Analyzer end
    -> Waltz.Syntax.Analyzer.Analyzer skip
    -> Waltz.Syntax.Analyzer.Analyzer [a]
someTill p end skip = do
    (:) <$> p <*> Waltz.Syntax.Common.manyTill p end skip

between
    :: Waltz.Syntax.Analyzer.Analyzer begin
    -> Waltz.Syntax.Analyzer.Analyzer end
    -> Waltz.Syntax.Analyzer.Analyzer a
    -> Waltz.Syntax.Analyzer.Analyzer skip
    -> Waltz.Syntax.Analyzer.Analyzer [a]
between begin end p skip = do
    begin
    Waltz.Syntax.Common.manyTill p end skip
{-# INLINE between #-}

expect
    :: Waltz.Syntax.Analyzer.Analyzer a
    -> Waltz.Syntax.Analyzer.Analyzer (Maybe a)
expect p = do
    skipManyTill
        do choice
            [ comment
            , whitespace
            , unexpectedToken
            ]
        do choice
            [ Just <$> p
            , Nothing <$ unexpectedEndOfInput
            ]
{-# INLINE expect #-}

keyword :: (MonadParsec e s m, Token s ~ Char) => Tokens s -> m (Tokens s)
keyword k = try do
    x <- chunk k

    choice
        [ eof
        , () <$ do lookAhead $ oneOf special
        ]

    pure x
{-# INLINE keyword #-}

unexpectedEndOfInput :: Waltz.Syntax.Analyzer.Analyzer ()
unexpectedEndOfInput = () <$ do
    position <- position

    eof

    lift do
        call @"waltz-bootstrap-compiler/diagnostic/send" do
            Waltz.Diagnostic.Diagnostic
                do inject $ Proxy @"error"
                do Waltz.Diagnostic.Location position position
                do "Syntax error, unexpected end of input"

unexpectedStatement :: Waltz.Syntax.Analyzer.Analyzer ()
unexpectedStatement = do
    single '\\'

    choice
        [ unexpectedEndOfInput
        , unexpectedToken
        ]

    skipMany
        do choice
            [ eof
            , () <$ do lookAhead $ oneOf [ '\r', '\n', '#', ',' ]
            ]

unexpectedToken :: Waltz.Syntax.Analyzer.Analyzer ()
unexpectedToken = () <$ do
    start <- position

    c <- anySingle

    end <- position

    lift do
        call @"waltz-bootstrap-compiler/diagnostic/send" do
            Waltz.Diagnostic.Diagnostic
                do inject $ Proxy @"error"
                do Waltz.Diagnostic.Location start end
                do "Syntax error, unexpected token '" ++ [c] ++ "'"

comment :: Waltz.Syntax.Analyzer.Analyzer ()
comment = () <$ do
    single '#'

    skipManyTill
        do anySingle
        do choice
            [ eof
            , () <$ do
                single '\n'
            , () <$ do
                single '\r'
                optional $ single '\n'
            ]

separator :: Waltz.Syntax.Analyzer.Analyzer ()
separator = () <$ single ','

whitespace :: Waltz.Syntax.Analyzer.Analyzer ()
whitespace = () <$ oneOf [ ' ', '\t', '\r', '\n' ]

special :: [Char]
special = 
    [ '\\'
    , ':'
    , ' '
    , '\t'
    , '#'
    , '\r'
    , '\n'
    , ','
    , ';'
    , '"'
    , '('
    , ')'
    , '['
    , ']'
    , '{'
    , '}'
    ]
