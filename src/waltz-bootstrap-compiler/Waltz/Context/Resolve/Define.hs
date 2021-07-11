module Waltz.Context.Resolve.Define where

import Waltz.Prelude
import Waltz.Declare
import qualified Data.HashMap.Strict
import qualified Waltz.Ast
import qualified Waltz.Ast.Semantic
import qualified Waltz.Error.BreakNotInBlock
import qualified Waltz.Error.ReturnNotInFunction
import qualified Waltz.Semantic.Analyzer

instance Define "waltz-bootstrap-compiler/context/resolve" where
    call' = fun \identifier -> do
        if Data.HashMap.Strict.member identifier reservedIdentifiers
            then return $ inject Waltz.Semantic.Analyzer.mock
            else pure ()

        case identifier of
            -- Handle global identifiers
            '/' : _ ->
                inject <$> call @"waltz-bootstrap-compiler/context/resolve-global" identifier
            -- Handle reserved identifiers
            "break" ->
                raise Waltz.Error.BreakNotInBlock.Type
            -- Handle local identifiers
            _ ->
                inject <$> call @"waltz-bootstrap-compiler/context/resolve-local" identifier

reservedIdentifiers :: Data.HashMap.Strict.HashMap String String
reservedIdentifiers = Data.HashMap.Strict.fromList
    [ (,) "infer"
        "/std/infer"
    , (,) "number"
        "/std/number"
    , (,) "string"
        "/std/string"
    , (,) "boolean"
        "/std/boolean"
    , (,) "true"
        "/std/boolean/true"
    , (,) "false"
        "/std/boolean/false"
    , (,) "not"
        "/std/boolean/not"
    , (,) "null"
        "/std/null"
    , (,) "size"
        "/std/size"
    , (,) "u8"
        "/std/u8"
    , (,) "u16"
        "/std/u16"
    , (,) "u32"
        "/std/u32"
    , (,) "u64"
        "/std/u64"
    , (,) "i8"
        "/std/i8"
    , (,) "i16"
        "/std/i16"
    , (,) "i32"
        "/std/i32"
    , (,) "i64"
        "/std/i64"
    , (,) "spawn"
        "/std/spawn"
    , (,) "await"
        "/std/await"
    , (,) "$"
        ""
    , (,) "="
        ""
    , (,) "/="
        ""
    , (,) "+"
        ""
    , (,) "-"
        ""
    , (,) "*"
        ""
    , (,) "/"
        ""
    , (,) "|"
        ""
    , (,) "&"
        ""
    , (,) "_"
        ""
    ]
