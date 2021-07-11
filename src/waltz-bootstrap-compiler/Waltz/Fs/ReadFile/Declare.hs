module Waltz.Fs.ReadFile.Declare where

import Waltz.Prelude
import qualified Data.Text

type instance Declare "waltz-bootstrap-compiler/fs/read-file" =
    '( String
    , Data.Text.Text
    , '[]
    )
