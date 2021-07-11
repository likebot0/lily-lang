module Waltz.Context.ResolveGlobal.Declare where

import Waltz.Prelude
import qualified Waltz.Ast
import qualified Waltz.Ast.Semantic

type instance Declare "waltz-bootstrap-compiler/context/resolve-global" =
    '( String
    , Waltz.Ast.Node "semantic-analyzed" "root"
    , '[ "waltz-bootstrap-compiler/diagnostic/send" ]
    )
