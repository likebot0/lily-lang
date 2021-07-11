module Waltz.Ast where

import Waltz.Prelude

data Node (a :: Symbol) (t :: Symbol) = Node
    { children :: Children a t
    , attributes :: Attributes a t
    }

type family Attributes (a :: Symbol) (t :: Symbol) :: *

type family Children (a :: Symbol) (t :: Symbol) :: *

type All a =
    [ Node a "block-expression"
    , Node a "discard"
    , Node a "expression"
    , Node a "grouped-expression"
    , Node a "identifier"
    , Node a "key-value"
    , Node a "lambda-expression"
    , Node a "literal/array"
    , Node a "literal/number"
    , Node a "literal/object"
    , Node a "literal/string"
    , Node a "root"
    , Node a "statement/base"
    , Node a "statement/if"
    , Node a "statement/include"
    , Node a "statement/let"
    , Node a "statement/with"
    , Node a "type-expression"
    ]

type Literal a =
    [ Node a "literal/array"
    , Node a "literal/number"
    , Node a "literal/object"
    , Node a "literal/string"
    ]

type ExpressionTerm a =
    [ Node a "block-expression"
    , Node a "grouped-expression"
    , Node a "identifier"
    , Node a "lambda-expression"
    , Node a "literal/array"
    , Node a "literal/number"
    , Node a "literal/object"
    , Node a "literal/string"
    , Node a "type-expression"
    ]

type Block a =
    [Union $ BlockChild a]

type BlockChild a =
    [ Node a "discard"
    , Node a "statement/if"
    , Node a "statement/let"
    , Node a "statement/with"
    ]

type instance Children a "block-expression" =
    Block a

type instance Children a "discard" =
    Node a "expression"

type instance Children a "expression" =
    [Union $ ExpressionTerm a]

type instance Children a "grouped-expression" =
    [Union $ ExpressionTerm a]

type instance Children a "identifier" =
    String

type instance Children a "key-value" =
    ( String
    , Node a "expression"
    )

type instance Children a "lambda-expression" =
    ( String
    , Block a
    )

type instance Children a "literal/array" =
    [Union $
        [ Node a "expression"
        , Node a "statement/include"
        ] ++ BlockChild a
    ]

type instance Children a "literal/number" =
    String

type instance Children a "literal/object" =
    [Union $
        [ Node a "key-value"
        , Node a "statement/include"
        ] ++ BlockChild a
    ]

type instance Children a "literal/string" =
    [Union
        [ String
        , Node a "grouped-expression"
        ]
    ]

type instance Children a "root" =
    [Union
        [ Node a "expression"
        , Node a "statement/base"
        ]
    ]

type instance Children a "statement/base" =
    Node a "identifier"

type instance Children a "statement/if" =
    ( Node a "expression"
    , Block a
    )

type instance Children a "statement/include" =
    Node a "expression"

type instance Children a "statement/let" =
    ( String
    , Node a "expression"
    )

type instance Children a "statement/with" =
    Node a "expression"

type instance Children a "type-expression" =
    Union $ ExpressionTerm a
