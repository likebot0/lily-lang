module Waltz.Syntax.Statement.LetStatement where

import Waltz.Prelude
import Text.Megaparsec
import qualified Waltz.Ast
import qualified Waltz.Ast.Syntax
import qualified Waltz.Syntax.Analyzer
import qualified Waltz.Syntax.Common
import qualified Waltz.Syntax.Expression

analyzer :: String -> Waltz.Syntax.Analyzer.Analyzer (Waltz.Ast.Node "syntax-analyzed" "statement/let")
analyzer end = Waltz.Syntax.Common.node do
    Waltz.Syntax.Common.keyword "\\let"

    Just identifier <- Waltz.Syntax.Common.expect $ some $ noneOf Waltz.Syntax.Common.special

    Waltz.Syntax.Common.expect
        do single ':'

    Just expression <- Waltz.Syntax.Common.expect
        do Waltz.Syntax.Expression.analyzer $ "," ++ end

    pure (identifier, expression)
