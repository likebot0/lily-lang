module Waltz.Syntax.LambdaExpression where

import Waltz.Prelude
import Data.List
import Text.Megaparsec
import qualified Data.Text
import qualified Waltz.Ast
import qualified Waltz.Ast.Syntax
import qualified Waltz.Syntax.Analyzer
import qualified Waltz.Syntax.Block
import qualified Waltz.Syntax.Common
import qualified Waltz.Syntax.Identifier

analyzer :: Waltz.Syntax.Analyzer.Analyzer (Waltz.Ast.Node "syntax-analyzed" "lambda-expression")
analyzer = Waltz.Syntax.Common.node do
    Waltz.Syntax.Common.keyword "fun"

    Waltz.Syntax.Common.expect
        do lookAhead $ noneOf $ Waltz.Syntax.Common.special \\ [ '{' ]

    identifier <- some $ noneOf Waltz.Syntax.Common.special

    Just body <- Waltz.Syntax.Common.expect
        do Waltz.Syntax.Block.analyzer

    pure (identifier, body)
