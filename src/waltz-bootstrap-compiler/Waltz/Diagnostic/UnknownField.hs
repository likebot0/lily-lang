module Waltz.Diagnostic.UnknownField where

import Waltz.Prelude
import Waltz.Declare
import Waltz.Ast
import Waltz.Ast.Syntax
import Waltz.Diagnostic

send x name =
    call @"waltz-bootstrap-compiler/diagnostic/send" $ Waltz.Diagnostic.Diagnostic
        do inject $ Proxy @"error"
        x
        do "Unknown field '" ++ name ++ "' on type object"
