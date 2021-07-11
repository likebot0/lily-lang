module Waltz.Ast.DumpJson.Declare where

import Waltz.Prelude
import Waltz.Ast

type instance Declare "waltz-bootstrap-compiler/ast/dump-json" =
    '(
        ( Union $ Waltz.Ast.All "semantic-analyzed"
        , String
        )
    , ()
    , '[]
    )
