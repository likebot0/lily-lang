module Waltz.Context.Resolve.Declare where

import Waltz.Prelude
import qualified Waltz.Ast
import qualified Waltz.Ast.Semantic

type instance Declare "waltz-bootstrap-compiler/context/resolve" =
    '( String
    , Union
        [ Waltz.Ast.Node "semantic-analyzed" "expression"
        , Waltz.Ast.Node "semantic-analyzed" "root"
        ]
    , '[ "waltz-bootstrap-compiler/diagnostic/send" ]
    )
