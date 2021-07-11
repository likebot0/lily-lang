module Waltz.Plugin.Clang.Ast where

import Waltz.Prelude
import Data.Aeson

data TranslationUnitDecl where
    TranslationUnitDecl ::
        [Node]
        -> TranslationUnitDecl

data Node where
    Node ::
        String
        -> Maybe String
        -> Maybe Type
        -> Maybe [Node]
        -> Node

data Type where
    Type ::
        Maybe String
        -> Type

instance ToJSON TranslationUnitDecl where
    toJSON (TranslationUnitDecl xs) = object
        [ "kind" .= do "TranslationUnitDecl" :: String
        , "inner" .= xs
        ]

instance ToJSON Node where
    toJSON (Node kind name typex xs) = object
        [ "kind" .= kind
        , "name" .= name
        , "type" .= typex
        , "inner" .= xs
        ]

instance ToJSON Type where
    toJSON (Type qualType) = object
        [ "qualType" .= qualType
        ]

instance FromJSON TranslationUnitDecl where
    parseJSON = withObject "TranslationUnitDecl" $ \v -> do
        TranslationUnitDecl
            <$> v .: "inner"

instance FromJSON Node where
    parseJSON = withObject "CModuleNode" $ \v -> do
        Node
            <$> v .: "kind"
            <*> v .:? "name"
            <*> v .:? "type"
            <*> v .:? "inner"

instance FromJSON Type where
    parseJSON = withObject "CModuleNodeType" $ \v -> do
        Type
            <$> v .:? "qualType"
