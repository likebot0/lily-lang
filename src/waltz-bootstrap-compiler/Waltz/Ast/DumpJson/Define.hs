module Waltz.Ast.DumpJson.Define where

import Waltz.Prelude
import Waltz.Declare
import Data.Aeson
import qualified Data.Aeson
import qualified Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as Bs
import qualified Waltz.Ast
import qualified Waltz.Ast.Semantic
import qualified Waltz.Ast.Syntax

instance Define "waltz-bootstrap-compiler/ast/dump-json" where
    call' = fun \(ast, outDir) -> do
        io $ Bs.writeFile (outDir ++ "/.wz.ast.json") $ encodePretty $ dispatch toJSON $ toInstance @ToJSON ast

encode :: ToJSON a => a -> Data.ByteString.Lazy.ByteString
encode = Data.Aeson.encode

encodePretty :: ToJSON a => a -> Data.ByteString.Lazy.ByteString
encodePretty = Data.Aeson.Encode.Pretty.encodePretty' Data.Aeson.Encode.Pretty.Config
    { Data.Aeson.Encode.Pretty.confIndent = Data.Aeson.Encode.Pretty.Tab
    , Data.Aeson.Encode.Pretty.confCompare = Data.Aeson.Encode.Pretty.keyOrder
        [ "type"
        , "attributes"
        , "location"
        , "start"
        , "end"
        , "line-number"
        , "column"
        , "children"
        , "condition"
        , "parameter"
        , "body"
        , "key"
        , "value"
        , "expression"
        ]
    , Data.Aeson.Encode.Pretty.confNumFormat = Data.Aeson.Encode.Pretty.Generic
    , Data.Aeson.Encode.Pretty.confTrailingNewline = False
    }

newtype Children' a k = Children' (Waltz.Ast.Children a k)

type AttributesToJSON a = 
    ( KnownSymbol a
    , ToJSON $ Waltz.Ast.Attributes a "block-expression"
    , ToJSON $ Waltz.Ast.Attributes a "discard"
    , ToJSON $ Waltz.Ast.Attributes a "key-value"
    , ToJSON $ Waltz.Ast.Attributes a "expression"
    , ToJSON $ Waltz.Ast.Attributes a "grouped-expression"
    , ToJSON $ Waltz.Ast.Attributes a "identifier"
    , ToJSON $ Waltz.Ast.Attributes a "lambda-expression"
    , ToJSON $ Waltz.Ast.Attributes a "literal/array"
    , ToJSON $ Waltz.Ast.Attributes a "literal/number"
    , ToJSON $ Waltz.Ast.Attributes a "literal/object"
    , ToJSON $ Waltz.Ast.Attributes a "literal/string"
    , ToJSON $ Waltz.Ast.Attributes a "root"
    , ToJSON $ Waltz.Ast.Attributes a "statement/base"
    , ToJSON $ Waltz.Ast.Attributes a "type-expression"
    , ToJSON $ Waltz.Ast.Attributes a "statement/if"
    , ToJSON $ Waltz.Ast.Attributes a "statement/include"
    , ToJSON $ Waltz.Ast.Attributes a "statement/let"
    , ToJSON $ Waltz.Ast.Attributes a "statement/with"
    )

instance (KnownSymbol a, KnownSymbol k, ToJSON $ Waltz.Ast.Attributes a k, ToJSON $ Children' a k) => ToJSON (Waltz.Ast.Node a k) where
    toJSON x =
        object
            [ "type" .= do
                toJSON do show $ Proxy @k
            , "attributes" .= do
                Waltz.Ast.attributes x
            , "children" .= do
                Children' @a @k $ Waltz.Ast.children x
            ]

instance AttributesToJSON a => ToJSON (Children' a "block-expression") where
    toJSON (Children' body) =
        toJSON do dispatch toJSON . toInstance @ToJSON <$> body

instance AttributesToJSON a => ToJSON (Children' a "discard") where
    toJSON (Children' expression) =
        toJSON expression

instance AttributesToJSON a => ToJSON (Children' a "expression") where
    toJSON (Children' terms) =
         toJSON do dispatch toJSON . toInstance @ToJSON <$> terms

instance AttributesToJSON a => ToJSON (Children' a "grouped-expression") where
    toJSON (Children' terms) =
         toJSON do dispatch toJSON . toInstance @ToJSON <$> terms

instance AttributesToJSON a => ToJSON (Children' a "identifier") where
    toJSON (Children' x) =
        toJSON x

instance AttributesToJSON a => ToJSON (Children' a "key-value") where
    toJSON (Children' (key, value)) =
        object
            [ "key" .= key
            , "value" .= value
            ]

instance AttributesToJSON a => ToJSON (Children' a "lambda-expression") where
    toJSON (Children' (argument, body)) =
        object
            [ "argument" .= argument
            , "body" .= toJSON do dispatch toJSON . toInstance @ToJSON <$> body
            ]

instance AttributesToJSON a => ToJSON (Children' a "literal/array") where
    toJSON (Children' body) =
        toJSON do dispatch toJSON . toInstance @ToJSON <$> body

instance AttributesToJSON a => ToJSON (Children' a "literal/number") where
    toJSON (Children' x) =
        toJSON x

instance AttributesToJSON a => ToJSON (Children' a "literal/object") where
    toJSON (Children' body) =
        toJSON do dispatch toJSON . toInstance @ToJSON <$> body

instance AttributesToJSON a => ToJSON (Children' a "literal/string") where
    toJSON (Children' x) =
        toJSON do dispatch toJSON . toInstance @ToJSON <$> x

instance AttributesToJSON a => ToJSON (Children' a "type-expression") where
    toJSON (Children' expression) =
        dispatch toJSON $ toInstance @ToJSON expression

instance AttributesToJSON a => ToJSON (Children' a "root") where
    toJSON (Children' body) =
        toJSON do dispatch toJSON . toInstance @ToJSON <$> body

instance AttributesToJSON a => ToJSON (Children' a "statement/base") where
    toJSON (Children' identifier) =
        toJSON identifier

instance AttributesToJSON a => ToJSON (Children' a "statement/if") where
    toJSON (Children' (condition, body)) =
        object
            [ "condition" .= condition
            , "body" .= toJSON do dispatch toJSON . toInstance @ToJSON <$> body
            ]

instance AttributesToJSON a => ToJSON (Children' a "statement/include") where
    toJSON (Children' expression) =
        toJSON expression

instance AttributesToJSON a => ToJSON (Children' a "statement/let") where
    toJSON (Children' (key, value)) =
        object
            [ "key" .= key
            , "value" .= value
            ]

instance AttributesToJSON a => ToJSON (Children' a "statement/with") where
    toJSON (Children' body) =
        toJSON body
