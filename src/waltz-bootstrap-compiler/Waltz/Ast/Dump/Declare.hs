module Waltz.Ast.Dump.Declare where

import Waltz.Prelude
import Waltz.Ast
import qualified Waltz.Ast.DumpJson.Declare

type instance Declare "waltz-bootstrap-compiler/ast/dump" =
    '( (Union (Waltz.Ast.All "semantic-analyzed"), String)
    , ()
    , Dependencies "waltz-bootstrap-compiler/ast/dump-json"
    )
