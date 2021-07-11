module Waltz.Semantic.Common where

import Waltz.Prelude
import Waltz.Declare
import qualified Waltz.Ast
import qualified Waltz.Ast.Semantic
import qualified Waltz.Data.Map
import qualified Waltz.Diagnostic
import qualified Waltz.Diagnostic.IndexOutOfBounds
import qualified Waltz.Diagnostic.IsNotCallable
import qualified Waltz.Diagnostic.MismatchedTypes
import qualified Waltz.Diagnostic.UnknownField
import qualified Waltz.Semantic.Analyzer

newScope f = do
    variableStoreRef <- new $ Waltz.Data.Map.empty @String @(Waltz.Ast.Node "semantic-analyzed" "expression")

    do
        f variableStoreRef

        |> withOverride @"waltz-bootstrap-compiler/context/resolve-local" do
            \super -> fun \identifier -> do
                variableStore <- get variableStoreRef

                Waltz.Data.Map.lookup identifier variableStore |> \case
                    Just x -> return Waltz.Semantic.Analyzer.mock
                    _ -> pure ()

                super identifier

loc :: Union (Waltz.Ast.ExpressionTerm "semantic-analyzed")
    -> (Waltz.Diagnostic.Location, Waltz.Ast.Semantic.Data "expression")
loc =
    do \(x :: Waltz.Ast.Node "semantic-analyzed" "block-expression") -> do
        let Waltz.Ast.Semantic.AttributesWithType l t = Waltz.Ast.attributes x

        (l, inject t)
    @>
    do \(x :: Waltz.Ast.Node "semantic-analyzed" "grouped-expression") -> do
        let Waltz.Ast.Semantic.AttributesWithType l (Waltz.Ast.Semantic.Type t) = Waltz.Ast.attributes x

        (l, t)
    @>
    do \(x :: Waltz.Ast.Node "semantic-analyzed" "identifier") -> do
        let Waltz.Ast.Semantic.AttributesWithType l (Waltz.Ast.Semantic.Type t) = Waltz.Ast.attributes x

        (l, t)
    @>
    do \(x :: Waltz.Ast.Node "semantic-analyzed" "lambda-expression") -> do
        let Waltz.Ast.Semantic.AttributesWithType l t = Waltz.Ast.attributes x

        (l, inject t)
    @>
    do \(x :: Waltz.Ast.Node "semantic-analyzed" "literal/array") -> do
        let Waltz.Ast.Semantic.AttributesWithType l t = Waltz.Ast.attributes x

        (l, inject t)
    @>
    do \(x :: Waltz.Ast.Node "semantic-analyzed" "literal/number") -> do
        let Waltz.Ast.Semantic.AttributesWithType l t = Waltz.Ast.attributes x

        (l, inject t)
    @>
    do \(x :: Waltz.Ast.Node "semantic-analyzed" "literal/object") -> do
        let Waltz.Ast.Semantic.AttributesWithType l t = Waltz.Ast.attributes x

        (l, inject t)
    @>
    do \(x :: Waltz.Ast.Node "semantic-analyzed" "literal/string") -> do
        let Waltz.Ast.Semantic.AttributesWithType l t = Waltz.Ast.attributes x

        (l, inject t)
    @>
    do \(x :: Waltz.Ast.Node "semantic-analyzed" "type-expression") -> do
        let Waltz.Ast.Semantic.AttributesWithType l t = Waltz.Ast.attributes x

        (l, inject t)
    @>
    typesExhausted

apply
    :: forall e. (LastMember IO e, Members Waltz.Semantic.Analyzer.Effects e)
    => Waltz.Ast.Semantic.Data "expression"
    -> Waltz.Ast.Semantic.Data "expression"
    -> Waltz.Diagnostic.Location
    -> Eff e (Waltz.Ast.Semantic.Data "expression")
apply x y l = x |>
    do fun \(x :: Waltz.Ast.Semantic.Type "lambda-expression") -> do
        return $ inject $ Waltz.Ast.Semantic.Type @"invalud" ()
    @>
    do fun \(x :: Waltz.Ast.Semantic.Type "literal/array") -> do
        y <- y |> is @(Waltz.Ast.Semantic.Type "invalud") |> \case
            Left y -> pure y
            Right y -> do
                return $ inject y

        y <- y |> isNot @(Waltz.Ast.Semantic.Type "literal/number") |> \case
            Left y -> pure y
            Right y -> do
                return $ inject $ Waltz.Ast.Semantic.Type @"invalud" ()

        let Waltz.Ast.Semantic.Type text = y

        i <- case toInt (read text :: Double) of
            Just x -> pure x
            Nothing -> do
                Waltz.Diagnostic.MismatchedTypes.send l

                return $ inject $ Waltz.Ast.Semantic.Type @"invalud" ()

        let Waltz.Ast.Semantic.Type elements = x

        let n = length elements

        when (i < 0 || i >= n) do
            Waltz.Diagnostic.IndexOutOfBounds.send l i n

            return $ inject $ Waltz.Ast.Semantic.Type @"invalud" ()

        let Waltz.Ast.Semantic.Type t = elements !! i

        return t
    @>
    do fun \(x :: Waltz.Ast.Semantic.Type "literal/number") -> do
        Waltz.Diagnostic.IsNotCallable.send l

        return $ inject $ Waltz.Ast.Semantic.Type @"invalud" ()
    @>
    do fun \(x :: Waltz.Ast.Semantic.Type "literal/object") -> do
        y <- y |> is @(Waltz.Ast.Semantic.Type "invalud") |> \case
            Left y -> pure y
            Right y -> do
                return $ inject y

        y <- y |> isNot @(Waltz.Ast.Semantic.Type "literal/string") |> \case
            Left y -> pure y
            Right y -> do
                Waltz.Diagnostic.MismatchedTypes.send l

                return $ inject $ Waltz.Ast.Semantic.Type @"invalud" ()

        let Waltz.Ast.Semantic.Type map = x

        let Waltz.Ast.Semantic.Type key = y

        Waltz.Data.Map.lookup key map |> \case
            Just x -> do
                let Waltz.Ast.Semantic.Type t = x

                return t
            Nothing -> do
                Waltz.Diagnostic.UnknownField.send l key

                return $ inject $ Waltz.Ast.Semantic.Type @"invalud" ()
    @>
    do fun \(x :: Waltz.Ast.Semantic.Type "literal/string") -> do
        y <- y |> is @(Waltz.Ast.Semantic.Type "invalud") |> \case
            Left y -> pure y
            Right _ -> do
                return $ inject $ Waltz.Ast.Semantic.Type @"invalud" ()

        y <- y |> isNot @(Waltz.Ast.Semantic.Type "literal/number") |> \case
            Left y -> pure y
            Right y -> do
                Waltz.Diagnostic.MismatchedTypes.send l

                return $ inject $ Waltz.Ast.Semantic.Type @"invalud" ()

        let Waltz.Ast.Semantic.Type text = y

        i <- case toInt (read text :: Double) of
            Just x -> pure x
            Nothing -> do
                Waltz.Diagnostic.MismatchedTypes.send l

                return $ inject $ Waltz.Ast.Semantic.Type @"invalud" ()

        let Waltz.Ast.Semantic.Type string = x

        let n = length string

        when (i < 0 || i >= n) do
            Waltz.Diagnostic.IndexOutOfBounds.send l i n

            return $ inject $ Waltz.Ast.Semantic.Type @"invalud" ()

        return $ inject $ Waltz.Ast.Semantic.Type @"literal/string" $ [string !! i]
    @>
    do fun \(x :: Waltz.Ast.Semantic.Type "invalud") -> do
        return $ inject x
    @>
    do fun \(x :: Waltz.Ast.Semantic.Type "unknown") -> do
        Waltz.Diagnostic.IsNotCallable.send l

        return $ inject $ Waltz.Ast.Semantic.Type @"invalud" ()
    @>
    typesExhausted

toInt :: Double -> Maybe Int
toInt i =
    if fromIntegral index == i
        then Just index
        else Nothing
    where
        index = ceiling i :: Int
