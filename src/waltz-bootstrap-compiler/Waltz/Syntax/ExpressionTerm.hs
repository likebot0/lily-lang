module Waltz.Syntax.ExpressionTerm where

import Waltz.Prelude
import Text.Megaparsec
import qualified Waltz.Ast
import qualified Waltz.Ast.Syntax
import qualified Waltz.Syntax.Analyzer
import qualified Waltz.Syntax.BlockExpression
import qualified Waltz.Syntax.GroupedExpression
import qualified Waltz.Syntax.Identifier
import qualified Waltz.Syntax.LambdaExpression
import qualified Waltz.Syntax.Literal.Array
import qualified Waltz.Syntax.Literal.Number
import qualified Waltz.Syntax.Literal.Object
import qualified Waltz.Syntax.Literal.String
import qualified Waltz.Syntax.TypeExpression

analyzer :: Waltz.Syntax.Analyzer.Analyzer (Union (Waltz.Ast.ExpressionTerm "syntax-analyzed"))
analyzer = choice
    [ inject <$> Waltz.Syntax.BlockExpression.analyzer
    , inject <$> Waltz.Syntax.GroupedExpression.analyzer
    , inject <$> Waltz.Syntax.LambdaExpression.analyzer
    , inject <$> Waltz.Syntax.Literal.Array.analyzer
    , inject <$> Waltz.Syntax.Literal.Number.analyzer
    , inject <$> Waltz.Syntax.Literal.Object.analyzer
    , inject <$> Waltz.Syntax.Literal.String.analyzer
    , inject <$> Waltz.Syntax.TypeExpression.analyzer
    , inject <$> Waltz.Syntax.Identifier.analyzer
    ]
