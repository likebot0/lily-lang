module Waltz.Context.ResolveLocal.Declare where

import Waltz.Prelude
import qualified Waltz.Ast
import qualified Waltz.Ast.Semantic

type instance Declare "waltz-bootstrap-compiler/context/resolve-local" =
    '( String
    , Waltz.Ast.Node "semantic-analyzed" "expression"
    , '[ "waltz-bootstrap-compiler/diagnostic/send" ]
    )
