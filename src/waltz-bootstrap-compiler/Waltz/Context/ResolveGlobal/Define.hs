module Waltz.Context.ResolveGlobal.Define where

import Waltz.Prelude
import Waltz.Declare
import qualified System.Directory
import qualified Text.Megaparsec
import qualified Waltz.Context.ResolveLocal.Define
import qualified Waltz.Ast
import qualified Waltz.Ast.Semantic
import qualified Waltz.Diagnostic
import qualified Waltz.Error.InternalCompilerError
import qualified Waltz.Semantic.Analyzer
import qualified Waltz.Semantic.Root
import qualified Waltz.Syntax.Root

instance Define "waltz-bootstrap-compiler/context/resolve-global" where
    call' =
        -- Verbose output
        (\next -> fun \identifier -> do
            io $ putStrLn $ "Loading " ++ identifier

            next identifier
        ) $
        -- AST dump
        (\next -> fun \identifier -> do
            ast <- next identifier

            outDirBase <- call @"waltz-bootstrap-compiler/config/get-out-dir" ()

            outDir <- pure $ outDirBase ++ identifier

            io $ System.Directory.createDirectoryIfMissing True outDir

            call @"waltz-bootstrap-compiler/ast/dump" (inject ast, outDir)

            return ast
        ) $
        fun \identifier -> do
            path <- call @"waltz-bootstrap-compiler/context/get-path-by-id" identifier

            do
                source <- call @"waltz-bootstrap-compiler/fs/read-file" path

                syntaxAst <- do
                    parsed <- Text.Megaparsec.runParserT'
                        do Waltz.Syntax.Root.analyzer
                        do initialState path 1 source

                    parsed |> \case
                        (_, Left x) -> raise $ Waltz.Error.InternalCompilerError.Type $ show x
                        (_, Right x) -> pure x

                Waltz.Semantic.Root.analyze syntaxAst

                |> withDefines @'[ "waltz-bootstrap-compiler/context/resolve-local" ]
                |> with @"waltz-bootstrap-compiler/get-current-src-path" do
                    fun \_ -> do
                        return path

initialState :: FilePath -> Int -> s -> Text.Megaparsec.State s e
initialState path tabWidth s = Text.Megaparsec.State
    { Text.Megaparsec.stateInput  = s
    , Text.Megaparsec.stateOffset = 0
    , Text.Megaparsec.statePosState = Text.Megaparsec.PosState
        { Text.Megaparsec.pstateInput = s
        , Text.Megaparsec.pstateOffset = 0
        , Text.Megaparsec.pstateSourcePos = Text.Megaparsec.initialPos path
        , Text.Megaparsec.pstateTabWidth = Text.Megaparsec.mkPos tabWidth
        , Text.Megaparsec.pstateLinePrefix = ""
        }
    , Text.Megaparsec.stateParseErrors = []
    }
