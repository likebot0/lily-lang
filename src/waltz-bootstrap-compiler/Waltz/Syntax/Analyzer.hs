module Waltz.Syntax.Analyzer where

import Waltz.Prelude
import Waltz.Declare
import Data.Void
import Data.Text
import qualified Text.Megaparsec

type ParsecT = Text.Megaparsec.ParsecT Void Text

type Analyzer a = forall e. (LastMember IO e, Members Effects e) => ParsecT (Eff e) a

type Effects =
    Raise : Requires Defined
    ++ Callable "waltz-bootstrap-compiler/diagnostic/send"
