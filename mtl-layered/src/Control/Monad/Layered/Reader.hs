{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.Layered.Reader where

import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Cont          as Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader        as ReaderT
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

class Monad m => MonadReader r (n :: N) m | m n -> r where
  ask    :: proxy n -> m r
  local  :: proxy n -> (r -> r) -> m a -> m a
  reader :: proxy n -> (r -> a) -> m a

-- Base instances {{{

instance MonadReader r Z ((->) r) where
  ask    _     = id
  local  _ f m = m . f
  reader _     = id

instance (Monad m, NoReader m) => MonadReader r Z (ReaderT r m) where
  ask    _ = ReaderT.ask
  local  _ = ReaderT.local
  reader _ = ReaderT.reader

instance (Monoid w, Monad m, NoReader m) => MonadReader r Z (LazyRWS.RWST r w s m) where
  ask    _ = LazyRWS.ask
  local  _ = LazyRWS.local
  reader _ = LazyRWS.reader

instance (Monoid w, Monad m, NoReader m) => MonadReader r Z (StrictRWS.RWST r w s m) where
  ask    _ = StrictRWS.ask
  local  _ = StrictRWS.local
  reader _ = StrictRWS.reader

-- }}}

-- Increasing instances {{{

instance MonadReader r' n m => MonadReader r (S n) (ReaderT r m) where
  ask    _ = ReaderT.ask
  local  _ = ReaderT.local
  reader _ = ReaderT.reader

instance (Monoid w, MonadReader r' n m) => MonadReader r (S n) (LazyRWS.RWST r w s m) where
  ask    _ = LazyRWS.ask
  local  _ = LazyRWS.local
  reader _ = LazyRWS.reader

instance (Monoid w, MonadReader r' n m) => MonadReader r (S n) (StrictRWS.RWST r w s m) where
  ask    _ = StrictRWS.ask
  local  _ = StrictRWS.local
  reader _ = StrictRWS.reader

-- }}}

-- Pass-through instances {{{

instance MonadReader r n m => MonadReader r n (ReaderT r' m) where
  ask    n = lift (ask n)
  local  n = ReaderT.mapReaderT . local n
  reader n = lift . reader n

instance (Monoid w, MonadReader r n m) => MonadReader r n (LazyRWS.RWST r' w s m) where
  ask    n = lift (ask n)
  local  n = LazyRWS.mapRWST . local n
  reader n = lift . reader n

instance (Monoid w, MonadReader r n m) => MonadReader r n (StrictRWS.RWST r' w s m) where
  ask    n = lift (ask n)
  local  n = StrictRWS.mapRWST . local n
  reader n = lift . reader n

-- }}}

-- Non-increasing instances {{{

instance MonadReader r' n m => MonadReader r' n (Cont.ContT r m) where
  ask    n = lift (ask n)
  local  n = Cont.liftLocal (ask n) (local n)
  reader n = lift . reader n

instance MonadReader r n m => MonadReader r n (ExceptT e m) where
  ask    n = lift (ask n)
  local  n = mapExceptT . (local n)
  reader n = lift . reader n

instance MonadReader r n m => MonadReader r n (IdentityT m) where
  ask    n = lift (ask n)
  local  n = mapIdentityT . (local n)
  reader n = lift . reader n

instance MonadReader r n m => MonadReader r n (ListT m) where
  ask    n = lift (ask n)
  local  n = mapListT . (local n)
  reader n = lift . reader n

instance MonadReader r n m => MonadReader r n (MaybeT m) where
  ask    n = lift (ask n)
  local  n = mapMaybeT . (local n)
  reader n = lift . reader n

instance MonadReader r n m => MonadReader r n (Lazy.StateT s m) where
  ask    n = lift (ask n)
  local  n = Lazy.mapStateT . (local n)
  reader n = lift . reader n

instance MonadReader r n m => MonadReader r n (Strict.StateT s m) where
  ask    n = lift (ask n)
  local  n = Strict.mapStateT . (local n)
  reader n = lift . reader n

instance (Monoid w, MonadReader r n m) => MonadReader r n (Lazy.WriterT w m) where
  ask    n = lift (ask n)
  local  n = Lazy.mapWriterT . (local n)
  reader n = lift . reader n

instance (Monoid w, MonadReader r n m) => MonadReader r n (Strict.WriterT w m) where
  ask    n = lift (ask n)
  local  n = Strict.mapWriterT . (local n)
  reader n = lift . reader n

-- }}}

-- NoReader instances {{{

class    NoReader (m :: * -> *)

instance (NoReader f, NoReader g) => NoReader (Compose f g)
instance                             NoReader (Constant r)
instance                             NoReader  Identity
instance (NoReader f, NoReader g) => NoReader (Product f g)
instance  NoReader f              => NoReader (Reverse f)
instance (NoReader f, NoReader g) => NoReader (Sum f g)

instance NoReader m => NoReader (Cont.ContT r m)
instance NoReader m => NoReader (ExceptT e m)
instance NoReader m => NoReader (IdentityT m)
instance NoReader m => NoReader (ListT m)
instance NoReader m => NoReader (MaybeT m)
instance NoReader m => NoReader (Lazy.StateT s m)
instance NoReader m => NoReader (Strict.StateT s m)
instance NoReader m => NoReader (Lazy.WriterT w m)
instance NoReader m => NoReader (Strict.WriterT w m)

-- }}}

