module Waltz.Diagnostic.IndexOutOfBounds where

import Waltz.Prelude
import Waltz.Declare
import Waltz.Ast
import Waltz.Ast.Syntax
import Waltz.Diagnostic

send x i n =
    call @"waltz-bootstrap-compiler/diagnostic/send" $ Waltz.Diagnostic.Diagnostic
        do inject $ Proxy @"error"
        x
        do "Index '" ++ show i ++ "' is out-of-bounds in array of length " ++ show n
