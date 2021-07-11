module Waltz.Semantic.Identifier where

import Waltz.Prelude
import qualified Waltz.Ast
import qualified Waltz.Ast.Semantic
import qualified Waltz.Ast.Syntax
import qualified Waltz.Diagnostic
import qualified Waltz.Semantic.Analyzer

analyze :: Waltz.Semantic.Analyzer.Analyze "identifier"
analyze x = do
    let Waltz.Ast.Node children (Waltz.Ast.Syntax.Attributes location) = x

    Waltz.Ast.Node children
        <$> Waltz.Ast.Semantic.AttributesWithType location
            <$> Waltz.Ast.Semantic.Type
                <$> (do
                    ast <- call @"waltz-bootstrap-compiler/context/resolve" $ children

                    pure $ ast |>
                        do \(x :: Waltz.Ast.Node "semantic-analyzed" "expression") -> do
                            let Waltz.Ast.Semantic.AttributesWithType _ t = Waltz.Ast.attributes x

                            inject $ Waltz.Ast.Semantic.Type @"unknown" ()
                        @>
                        do \(x :: Waltz.Ast.Node "semantic-analyzed" "root") ->
                            inject $ Waltz.Ast.Semantic.Type @"unknown" ()
                        @>
                        typesExhausted

                    |> withExceptionHandler do
                        fun \e -> do
                            call @"waltz-bootstrap-compiler/diagnostic/send" $ Waltz.Diagnostic.Diagnostic
                                do inject $ Proxy @"error"
                                do Waltz.Ast.Syntax.location $ Waltz.Ast.attributes x
                                do show e
                            
                            pure $ inject $ Waltz.Ast.Semantic.Type @"unknown" ()
                )
