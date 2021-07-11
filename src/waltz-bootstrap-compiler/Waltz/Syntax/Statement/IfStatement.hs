module Waltz.Syntax.Statement.IfStatement where

import Waltz.Prelude
import Data.List
import Text.Megaparsec
import qualified Waltz.Ast
import qualified Waltz.Ast.Syntax
import qualified Waltz.Syntax.Analyzer
import {-# SOURCE #-} qualified Waltz.Syntax.Block
import qualified Waltz.Syntax.Common
import qualified Waltz.Syntax.Expression

analyzer :: String -> Waltz.Syntax.Analyzer.Analyzer (Waltz.Ast.Node "syntax-analyzed" "statement/if")
analyzer end = Waltz.Syntax.Common.node do
    Waltz.Syntax.Common.keyword "\\if"

    Just expression <- Waltz.Syntax.Common.expect
        do Waltz.Syntax.Expression.analyzer $ "," ++ end

    optional Waltz.Syntax.Common.separator

    Just body <- Waltz.Syntax.Common.expect
        do Waltz.Syntax.Block.analyzer

    Waltz.Syntax.Common.expect
        do choice
            [ eof
            , () <$ do lookAhead $ oneOf $ [ '\r', '\n', '#', ',' ] ++ end
            ]

    pure (expression, body)
