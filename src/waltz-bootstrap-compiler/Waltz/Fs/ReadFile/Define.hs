module Waltz.Fs.ReadFile.Define where

import Waltz.Prelude
import Waltz.Declare
import Data.Text.IO

instance Define "waltz-bootstrap-compiler/fs/read-file" where
    call' = fun \path -> do
        io $ Data.Text.IO.readFile path
