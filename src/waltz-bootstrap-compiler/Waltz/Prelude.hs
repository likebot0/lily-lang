module Waltz.Prelude
    ( Data.IORef.IORef
    , module Control.Monad.Freer
    , module Control.Monad.Trans
    , module GHC.TypeLits
    , module Prelude
    , module Waltz.Data.Instance
    , module Waltz.Data.OpenUnion
    , module Waltz.Data.Proxy
    , module Waltz.Data.Type.List
    , module Waltz.Prelude
    ) where

import Prelude hiding (return)
import Control.Monad.Freer (Eff, Members, LastMember)
import Control.Monad.Trans (lift)
import Data.Kind (Constraint)
import GHC.TypeLits (Symbol, KnownSymbol)
import Waltz.Data.Instance
import Waltz.Data.OpenUnion
import Waltz.Data.Proxy
import Waltz.Data.Type.List
import qualified Control.Monad.Freer
import qualified Control.Monad.Freer.Internal
import qualified Data.IORef
import qualified Waltz.Monad

type Fun s =
    forall e. (LastMember IO e, Members (Raise : Requires Defined ++ Callable s) e) =>
    Input s -> Eff e (Output s)

newtype Fun' :: Symbol -> * where
    Fun' :: forall s. Fun s -> Fun' s

data Raise :: * -> * where
    Raise :: forall a. Instance Show -> Raise a

data Require :: Symbol -> * -> * where
    Require :: forall s. Require s (Fun' s)

type family Requires (a :: [ Symbol ]) :: [ * -> * ] where
    Requires '[] = '[]
    Requires (s : ss) = Require s : Requires ss

newtype Return a (r :: *) where
    Return :: a -> Return a b

type family Declare (s :: Symbol) :: (*, *, [ Symbol ])

type family Input (a :: k) :: * where
    Input '(b, _, _) = b
    Input (s :: Symbol) = Input (Declare s)

type family Output (a :: k) :: * where
    Output '(_, b, _) = b
    Output (s :: Symbol) = Output (Declare s)

type family Dependencies (a :: k) :: [ Symbol ] where
    Dependencies '(_, _, b) = b
    Dependencies (s :: Symbol) = Dependencies (Declare s)

type Callable s = Require s : Requires (Dependencies s)

class Define (s :: Symbol) where
    call' :: Fun s

type family Defines (s :: [ Symbol ]) :: Constraint where
    Defines '[] = ()
    Defines (s : ss) = (Define s, Defines ss)

call :: forall s. Fun s
call x =
    require @s >>= \(Fun' f) -> f x
{-# INLINE call #-}

fun :: (a -> Eff (Return b : e) b) -> a -> Eff e b
fun f a = do
    Control.Monad.Freer.Internal.handleRelay
        pure
        do \(Return x) _ -> pure x
        do f a
{-# INLINE fun #-}

io :: forall e a. LastMember IO e => IO a -> Eff e a
io = Control.Monad.Freer.sendM
{-# INLINE io #-}

raise :: forall e a b. (Control.Monad.Freer.Member Raise e, Show a) => a -> Eff e b
raise =
    Control.Monad.Freer.send . Raise . Instance
{-# INLINE raise #-}

return :: forall e a b. a -> Eff (Return a : e) b
return =
    Control.Monad.Freer.send . Return
{-# INLINE return #-}

require :: forall s e. (Control.Monad.Freer.Member (Require s) e) => Eff e (Fun' s)
require =
    Control.Monad.Freer.send Require
{-# INLINE require #-}

with :: forall s. Fun s -> forall e a. Eff (Require s : e) a -> Eff e a
with f m = do
    Control.Monad.Freer.Internal.handleRelay
        pure
        do \Require k -> k (Fun' f)
        m
{-# INLINE with #-}

withOverride :: forall s. (Fun s -> Fun s) -> forall e a. Control.Monad.Freer.Member (Require s) e => Eff (Require s : e) a -> Eff e a
withOverride f m = do
    Fun' g <- require @s

    Control.Monad.Freer.Internal.handleRelay
        pure
        do \Require k -> k (Fun' $ f g)
        m
{-# INLINE withOverride #-}

withExceptionHandler :: forall e a. (forall b. Show b => b -> Eff e a) -> Eff (Raise : e) a -> Eff e a
withExceptionHandler f =
    Control.Monad.Freer.Internal.handleRelay
        pure
        \(Raise (Instance x)) _ -> f x
{-# INLINE withExceptionHandler #-}

withIO :: forall e a. Eff '[IO] a -> IO a
withIO = Control.Monad.Freer.runM
{-# INLINE withIO #-}

class WithDefines (ss :: [ Symbol ]) where
    withDefines :: forall e a. Defines ss => Eff (Requires ss ++ e) a -> Eff e a

instance WithDefines '[] where
    withDefines = id

instance WithDefines ss => WithDefines (s : ss) where
    withDefines = withDefines @ss . (with @s $ call' @s)

type Defined =
    [ "waltz-bootstrap-compiler/ast/dump"
    , "waltz-bootstrap-compiler/ast/dump-json"
    , "waltz-bootstrap-compiler/config/get-entrypoints"
    , "waltz-bootstrap-compiler/config/get-opt-dirs"
    , "waltz-bootstrap-compiler/config/get-out-dir"
    , "waltz-bootstrap-compiler/config/get-src-dirs"
    , "waltz-bootstrap-compiler/context/get-path-by-id"
    , "waltz-bootstrap-compiler/context/resolve"
    , "waltz-bootstrap-compiler/context/resolve-global"
    , "waltz-bootstrap-compiler/context/resolve-local"
    , "waltz-bootstrap-compiler/fs/read-file"
    ]

when p f =
    if p then
        f
    else
        pure ()

infixl 1 |>

(|>) :: a -> (a -> b) -> b
(|>) a f = f a
{-# INLINE (|>) #-}

new :: forall e a. LastMember IO e => a -> Eff e (Data.IORef.IORef a)
new = io . Data.IORef.newIORef
{-# INLINE new #-}

get :: forall e a. LastMember IO e => Data.IORef.IORef a -> Eff e a
get = io . Data.IORef.readIORef
{-# INLINE get #-}

set :: forall e a. LastMember IO e => Data.IORef.IORef a -> a -> Eff e ()
set ref = io . Data.IORef.writeIORef ref
{-# INLINE set #-}

type family (f :: k -> k') $ (a :: k) :: k' where
    f $ a = f a
infixl 1 $
