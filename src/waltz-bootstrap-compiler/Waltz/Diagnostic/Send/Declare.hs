module Waltz.Diagnostic.Send.Declare where

import Waltz.Prelude
import qualified Waltz.Diagnostic

type instance Declare "waltz-bootstrap-compiler/diagnostic/send" =
    '( Waltz.Diagnostic.Diagnostic
    , ()
    , '[ "waltz-bootstrap-compiler/get-current-src-path"
      ]
    )
