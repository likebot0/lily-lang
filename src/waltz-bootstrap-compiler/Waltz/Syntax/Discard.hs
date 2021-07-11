module Waltz.Syntax.Discard where

import Waltz.Prelude
import Data.List
import Text.Megaparsec
import qualified Waltz.Ast
import qualified Waltz.Ast.Syntax
import qualified Waltz.Syntax.Analyzer
import qualified Waltz.Syntax.Common
import qualified Waltz.Syntax.Expression

analyzer :: String -> Waltz.Syntax.Analyzer.Analyzer (Waltz.Ast.Node "syntax-analyzed" "discard")
analyzer end = Waltz.Syntax.Common.node do
    single ':'

    Just x <- Waltz.Syntax.Common.expect
        do Waltz.Syntax.Expression.analyzer $ "," ++ end

    pure x
