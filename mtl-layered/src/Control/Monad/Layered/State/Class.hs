{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.Layered.State.Class where

import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Cont          as Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.Reader        as Reader
import qualified Control.Monad.Trans.RWS.Lazy      as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict    as StrictRWS
import qualified Control.Monad.Trans.State.Lazy    as Lazy
import qualified Control.Monad.Trans.State.Strict  as Strict
import qualified Control.Monad.Trans.Writer.Lazy   as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Data.Nat
import Data.Monoid (Monoid)
import Data.Functor.Compose
import Data.Functor.Constant
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Reverse
import Data.Functor.Sum

class Monad m => MonadState s (n :: N) m | m n -> s where
  get :: proxy n -> m s
  get n   = state n $ \s -> (s,s)
  ----
  put :: proxy n -> s -> m ()
  put n s = state n $ \_ -> ((),s)
  ----
  state  :: proxy n -> (s -> (a,s)) -> m a
  state n f = do
    s <- get n
    let ~(a,s') = f s
    put   n s'
    return a

-- Base instances {{{

instance (Monad m, NoState m) => MonadState s Z (Lazy.StateT s m) where
  get    _ = Lazy.get
  put    _ = Lazy.put
  state  _ = Lazy.state

instance (Monad m, NoState m) => MonadState s Z (Strict.StateT s m) where
  get    _ = Strict.get
  put    _ = Strict.put
  state  _ = Strict.state

instance (Monoid w, Monad m, NoState m) => MonadState s Z (LazyRWS.RWST r w s m) where
  get    _ = LazyRWS.get
  put    _ = LazyRWS.put
  state  _ = LazyRWS.state

instance (Monoid w, Monad m, NoState m) => MonadState s Z (StrictRWS.RWST r w s m) where
  get    _ = StrictRWS.get
  put    _ = StrictRWS.put
  state  _ = StrictRWS.state

-- }}}

-- Increasing instances {{{

instance MonadState s' n m => MonadState s (S n) (Lazy.StateT s m) where
  get    _ = Lazy.get
  put    _ = Lazy.put
  state  _ = Lazy.state

instance MonadState s' n m => MonadState s (S n) (Strict.StateT s m) where
  get    _ = Strict.get
  put    _ = Strict.put
  state  _ = Strict.state

instance (Monoid w, MonadState s' n m) => MonadState s (S n) (LazyRWS.RWST r w s m) where
  get    _ = LazyRWS.get
  put    _ = LazyRWS.put
  state  _ = LazyRWS.state

instance (Monoid w, MonadState s' n m) => MonadState s (S n) (StrictRWS.RWST r w s m) where
  get    _ = StrictRWS.get
  put    _ = StrictRWS.put
  state  _ = StrictRWS.state

-- }}}

-- Pass-through instances {{{

instance MonadState s n m => MonadState s n (Lazy.StateT s' m) where
  get    n = lift $ get n
  put    n = lift . put n
  state  n = lift . state n

instance MonadState s n m => MonadState s n (Strict.StateT s' m) where
  get    n = lift $ get n
  put    n = lift . put n
  state  n = lift . state n

instance (Monoid w, MonadState s n m) => MonadState s n (LazyRWS.RWST r w s' m) where
  get    n = lift $ get n
  put    n = lift . put n
  state  n = lift . state n

instance (Monoid w, MonadState s n m) => MonadState s n (StrictRWS.RWST r w s' m) where
  get    n = lift $ get n
  put    n = lift . put n
  state  n = lift . state n

-- }}}

-- Non-increasing instances {{{

instance MonadState s' n m => MonadState s' n (Cont.ContT r m) where
  get    n = lift $ get n
  put    n = lift . put n
  state  n = lift . state n

instance MonadState s n m => MonadState s n (ExceptT e m) where
  get    n = lift $ get n
  put    n = lift . put n
  state  n = lift . state n

instance MonadState s n m => MonadState s n (IdentityT m) where
  get    n = lift $ get n
  put    n = lift . put n
  state  n = lift . state n

instance MonadState s n m => MonadState s n (ListT m) where
  get    n = lift $ get n
  put    n = lift . put n
  state  n = lift . state n

instance MonadState s n m => MonadState s n (MaybeT m) where
  get    n = lift $ get n
  put    n = lift . put n
  state  n = lift . state n

instance MonadState s n m => MonadState s n (Reader.ReaderT r m) where
  get    n = lift $ get n
  put    n = lift . put n
  state  n = lift . state n

instance (Monoid w, MonadState s n m) => MonadState s n (Lazy.WriterT w m) where
  get    n = lift $ get n
  put    n = lift . put n
  state  n = lift . state n

instance (Monoid w, MonadState s n m) => MonadState s n (Strict.WriterT w m) where
  get    n = lift $ get n
  put    n = lift . put n
  state  n = lift . state n

-- }}}

-- NoState instances {{{

class    NoState (m :: * -> *)

instance (NoState f, NoState g) => NoState (Compose f g)
instance                           NoState (Constant r)
instance                           NoState  Identity
instance (NoState f, NoState g) => NoState (Product f g)
instance  NoState f             => NoState (Reverse f)
instance (NoState f, NoState g) => NoState (Sum f g)

instance NoState m => NoState (Cont.ContT r m)
instance NoState m => NoState (ExceptT e m)
instance NoState m => NoState (IdentityT m)
instance NoState m => NoState (ListT m)
instance NoState m => NoState (MaybeT m)
instance NoState m => NoState (Reader.ReaderT r m)
instance NoState m => NoState (Lazy.WriterT w m)
instance NoState m => NoState (Strict.WriterT w m)

-- }}}

