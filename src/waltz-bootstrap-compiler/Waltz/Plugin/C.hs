module Waltz.Context.Resolve.C where

import Waltz.Prelude
import Waltz.Declare
import qualified Data.Aeson
import qualified Data.ByteString.Lazy.Char8
import qualified Data.List
import qualified Data.List.Split
import qualified Waltz.Ast
import qualified Waltz.Ast.Semantic
import qualified Waltz.Error.CannotResolve
import qualified System.Process

-- withDefines = with @"waltz-bootstrap-compiler/context/resolve/c" $ \identifier -> do
--     pure $ Waltz.Ast.Node () ()

    -- do
    --     case Data.List.Split.splitOn "/" identifier of
    --         [_, _, identifier] -> do
    --             case identifier of
    --                 "pointer" -> 
    --                     pure $ Waltz.Ast.Node () identifier
    --                 _ -> 
    --                     raise $ Waltz.Error.CannotResolve.Type identifier

    --         [_, _, includePath, identifier] -> do
    --             let command = "clang"

    --             let arguments = ["-x", "c", "-Xclang", "-ast-dump=json", "-fsyntax-only", "-fno-color-diagnostics", "-"]

    --             output <- io $ System.Process.readProcess command arguments $ "#include <" ++ includePath ++ ">"

    --             nodes <- case Data.Aeson.decode (Data.ByteString.Lazy.Char8.pack output) of
    --                 Nothing -> raise $ Waltz.Error.CannotResolve.Type identifier
    --                 Just (Plugin.Clang.TranslationUnitDecl x) -> pure x

    --             let match = \case
    --                     Plugin.Clang.Node _ (Just name) _ _ -> name == identifier
    --                     _ -> False 

    --             node <- case Data.List.find match nodes of
    --                 Nothing -> raise $ Waltz.Error.CannotResolve.Type identifier
    --                 Just x -> pure x

    --             case node of
    --                 (Plugin.Clang.Node _ _ (Just (Plugin.Clang.Type (Just t))) _) ->
    --                     pure $ Waltz.Ast.Node () t

    --         _ -> do
    --             raise $ Waltz.Error.CannotResolve.Type identifier

    --     |> with @"waltz-bootstrap-compiler/get-current-src-path" do
    --         fun \_ -> do
    --             return identifier
