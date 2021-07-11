module Waltz.Ast.Dump.Define where

import Waltz.Prelude
import Waltz.Declare

instance Define "waltz-bootstrap-compiler/ast/dump" where
    call' = fun \ast -> do
        call @"waltz-bootstrap-compiler/ast/dump-json" ast
