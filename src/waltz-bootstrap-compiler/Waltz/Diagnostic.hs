module Waltz.Diagnostic where

import Waltz.Prelude
import Data.Aeson

data Diagnostic =
    Diagnostic
        Severity
        Location
        String

type Severity = Union
    [ Proxy "error"
    , Proxy "warning"
    , Proxy "information"
    , Proxy "hint"
    ]

data Location =
    Location
        Position
        Position

data Position =
    Position
        Int
        Int

instance Show Diagnostic where
    show (Diagnostic severity (Location (Position l c) _) message) =
        mconcat
            [ "("
            , show l
            , ","
            , show c
            , "): "
            , dispatch show $ toInstance @Show $ severity
            , ": "
            , message
            ]

instance Data.Aeson.ToJSON Diagnostic where
    toJSON (Diagnostic severity location message) =
        object
            [ "severity" .= do dispatch show $ toInstance @Show $ severity
            , "location" .= location
            , "message" .= message 
            ]

instance Data.Aeson.ToJSON Location where
    toJSON (Location start end) =
        object
            [ "start" .= start
            , "end" .= end
            ]

instance Data.Aeson.ToJSON Position where
    toJSON (Position lineNumber column) =
        object
            [ "line-number" .= lineNumber
            , "column" .= column
            ]
