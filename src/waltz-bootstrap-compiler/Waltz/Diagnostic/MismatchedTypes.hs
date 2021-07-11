module Waltz.Diagnostic.MismatchedTypes where

import Waltz.Prelude
import Waltz.Declare
import Waltz.Ast
import Waltz.Ast.Semantic
import Waltz.Ast.Syntax
import Waltz.Diagnostic

send x =
    call @"waltz-bootstrap-compiler/diagnostic/send" $ Waltz.Diagnostic.Diagnostic
        do inject $ Proxy @"error"
        x
        "Mismatched types"
