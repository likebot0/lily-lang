module Main where

import Waltz.Prelude
import Waltz.Declare
import qualified Data.HashMap.Strict
import qualified System.IO
import qualified Waltz.Ast.Dump.Define
import qualified Waltz.Ast.DumpJson.Define
import qualified Waltz.Config.GetEntrypoints.Define
import qualified Waltz.Config.GetOptDirs.Define
import qualified Waltz.Config.GetOutDir.Define
import qualified Waltz.Config.GetSrcDirs.Define
import qualified Waltz.Context.GetPathById.Define
import qualified Waltz.Context.Resolve.Define
import qualified Waltz.Context.ResolveGlobal.Define
import qualified Waltz.Context.ResolveLocal.Define
import qualified Waltz.Fs.ReadFile.Define

main :: IO ()
main = do
    entryPoints <- call @"waltz-bootstrap-compiler/config/get-entrypoints" ()

    cacheStoreRef <- new $ Data.HashMap.Strict.empty @(Input "waltz-bootstrap-compiler/context/resolve-global") @(Output "waltz-bootstrap-compiler/context/resolve-global")

    do 
        (`mapM_` entryPoints) $ \entryPoint -> do
            ast <- call @"waltz-bootstrap-compiler/context/resolve" entryPoint

            pure ()

        pure ()

        -- Caching
        |> withOverride @"waltz-bootstrap-compiler/context/resolve-global" do
            \super -> fun \identifier -> do
                cacheStore <- get cacheStoreRef

                Data.HashMap.Strict.lookup identifier cacheStore |> \case
                    Just x -> return x
                    _ -> pure ()

                result <- super identifier

                set cacheStoreRef $ Data.HashMap.Strict.insert identifier result cacheStore

                return result

    |> with @"waltz-bootstrap-compiler/diagnostic/send" do
        fun \x -> do
            path <- call @"waltz-bootstrap-compiler/get-current-src-path" ()

            io $ System.IO.hPutStrLn System.IO.stderr $ path ++ show x

    |> withExceptionHandler do
        fun \x -> do
            io $ System.IO.hPrint System.IO.stderr x

    |> withDefines @Defined
    |> withIO
