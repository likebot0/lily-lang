module Waltz.Diagnostic.CannotResolve where

import Waltz.Prelude
import Waltz.Declare
import Waltz.Ast
import Waltz.Ast.Syntax
import Waltz.Diagnostic

send x =
    call @"waltz-bootstrap-compiler/diagnostic/send" $ Waltz.Diagnostic.Diagnostic
        do inject $ Proxy @"error"
        do Waltz.Ast.Syntax.location $ Waltz.Ast.attributes x
        do "Cannot resolve '" ++ Waltz.Ast.children x ++ "'"
