module Waltz.Syntax.KeyValue where

import Waltz.Prelude
import Data.List
import Text.Megaparsec
import qualified Waltz.Ast
import qualified Waltz.Ast.Syntax
import qualified Waltz.Syntax.Analyzer
import qualified Waltz.Syntax.Common
import qualified Waltz.Syntax.Expression

analyzer :: String -> Waltz.Syntax.Analyzer.Analyzer (Waltz.Ast.Node "syntax-analyzed" "key-value")
analyzer end = Waltz.Syntax.Common.node do
    identifier <- some $ noneOf Waltz.Syntax.Common.special

    Waltz.Syntax.Common.expect
        do single ':'

    Just expression <- Waltz.Syntax.Common.expect
        do Waltz.Syntax.Expression.analyzer $ "," ++ end

    pure (identifier, expression)
