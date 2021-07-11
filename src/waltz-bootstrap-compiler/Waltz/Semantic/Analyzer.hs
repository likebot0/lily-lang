module Waltz.Semantic.Analyzer where

import Waltz.Prelude
import Waltz.Declare
import qualified Waltz.Ast
import qualified Waltz.Ast.Semantic
import qualified Waltz.Ast.Syntax
import qualified Waltz.Diagnostic

type Analyze s = forall e. (LastMember IO e, Members Effects e) => Waltz.Ast.Node "syntax-analyzed" s -> Eff e (Waltz.Ast.Node "semantic-analyzed" s)

type Effects =
    Raise : Requires Defined
    ++ Callable "waltz-bootstrap-compiler/diagnostic/send"
    ++ Callable "waltz-bootstrap-compiler/context/resolve"

mock :: Waltz.Ast.Node "semantic-analyzed" "expression"
mock = Waltz.Ast.Node
    do []
    do Waltz.Ast.Semantic.AttributesWithType
        do Waltz.Diagnostic.Location
            do Waltz.Diagnostic.Position 1 1
            do Waltz.Diagnostic.Position 1 1
        do Waltz.Ast.Semantic.Type
            do inject $ Waltz.Ast.Semantic.Type @"lambda-expression" ()
