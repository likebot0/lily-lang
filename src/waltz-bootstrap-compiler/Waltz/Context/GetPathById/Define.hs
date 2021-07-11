module Waltz.Context.GetPathById.Define where

import Waltz.Prelude
import Waltz.Declare
import qualified Waltz.Error.CannotResolve
import qualified System.Directory

instance Define "waltz-bootstrap-compiler/context/get-path-by-id" where
    call' = fun \identifier -> do
        identifier <- identifier |> \case
            '~' : '/' : xs -> pure xs
            x -> pure x

        srcDirs <- call @"waltz-bootstrap-compiler/config/get-src-dirs" ()

        io (System.Directory.findFile ((++ identifier) <$> srcDirs) ".wz") >>= \case
            Just x -> return $ normalize x
            _ -> pure ()

        optDirs <- call @"waltz-bootstrap-compiler/config/get-opt-dirs" ()

        optSrcDirs <- pure do
            optDir <- optDirs

            srcDir <- srcDirs

            pure $ optDir ++ "/" ++ srcDir

        io (System.Directory.findFile ((++ identifier) <$> optSrcDirs) ".wz") >>= \case
            Just x -> return $ normalize x
            _ -> pure ()

        raise $ Waltz.Error.CannotResolve.Type identifier

normalize :: String -> String
normalize = ((\x -> if x == '\\' then '/' else x) <$>)
