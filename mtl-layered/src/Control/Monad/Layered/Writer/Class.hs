{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.Layered.Writer.Class where

import Control.Arrow (first)
import Control.Monad (liftM)
import Control.Monad.Trans.Class
import Control.Monad.Signatures
import qualified Control.Monad.Trans.Except        as Except
import qualified Control.Monad.Trans.Identity      as Identity
import qualified Control.Monad.Trans.Maybe         as Maybe
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

class (Monoid w, Monad m) => MonadWriter w (n :: N) m | m n -> w where
  writer :: proxy n -> (a,w) -> m a
  writer n ~(a,w) = do
    tell n w
    return a
  ----
  tell :: proxy n -> w -> m ()
  tell n w = writer n ((),w)
  ----
  listen :: proxy n -> m a -> m (a,w)
  ----
  pass :: proxy n -> m (a,w -> w) -> m a

-- Base instances {{{

instance (Monoid w, Monad m, NoWriter m) => MonadWriter w Z (Lazy.WriterT w m) where
  writer _ = Lazy.writer
  tell   _ = Lazy.tell
  listen _ = Lazy.listen
  pass   _ = Lazy.pass

instance (Monoid w, Monad m, NoWriter m) => MonadWriter w Z (Strict.WriterT w m) where
  writer _ = Strict.writer
  tell   _ = Strict.tell
  listen _ = Strict.listen
  pass   _ = Strict.pass

instance (Monoid w, Monad m, NoWriter m) => MonadWriter w Z (LazyRWS.RWST r w s m) where
  writer _ = LazyRWS.writer
  tell   _ = LazyRWS.tell
  listen _ = LazyRWS.listen
  pass   _ = LazyRWS.pass

instance (Monoid w, Monad m, NoWriter m) => MonadWriter w Z (StrictRWS.RWST r w s m) where
  writer _ = StrictRWS.writer
  tell   _ = StrictRWS.tell
  listen _ = StrictRWS.listen
  pass   _ = StrictRWS.pass

-- }}}

-- Increasing instances {{{

instance (Monoid w, MonadWriter w' n m) => MonadWriter w (S n) (Lazy.WriterT w m) where
  writer _ = Lazy.writer
  tell   _ = Lazy.tell
  listen _ = Lazy.listen
  pass   _ = Lazy.pass

instance (Monoid w, MonadWriter w' n m) => MonadWriter w (S n) (Strict.WriterT w m) where
  writer _ = Strict.writer
  tell   _ = Strict.tell
  listen _ = Strict.listen
  pass   _ = Strict.pass

instance (Monoid w, MonadWriter w' n m) => MonadWriter w (S n) (LazyRWS.RWST r w s m) where
  writer _ = LazyRWS.writer
  tell   _ = LazyRWS.tell
  listen _ = LazyRWS.listen
  pass   _ = LazyRWS.pass

instance (Monoid w, MonadWriter w' n m) => MonadWriter w (S n) (StrictRWS.RWST r w s m) where
  writer _ = StrictRWS.writer
  tell   _ = StrictRWS.tell
  listen _ = StrictRWS.listen
  pass   _ = StrictRWS.pass

-- }}}

-- Pass-through instances {{{

instance (Monoid w, Monoid w', MonadWriter w n m) => MonadWriter w n (Lazy.WriterT w' m) where
  writer n = lift . writer n
  tell   n = lift . tell n
  listen n = liftListen $ listen n
  pass   n = liftPass $ pass n

instance (Monoid w, Monoid w', MonadWriter w n m) => MonadWriter w n (Strict.WriterT w' m) where
  writer n = lift . writer n
  tell   n = lift . tell n
  listen n = liftListen' $ listen n
  pass   n = liftPass' $ pass n

instance (Monoid w, Monoid w', MonadWriter w n m) => MonadWriter w n (LazyRWS.RWST r w' s m) where
  writer n = lift . writer n
  tell   n = lift . tell n
  listen n = liftListenRWS $ listen n
  pass   n = liftPassRWS $ pass   n

instance (Monoid w, Monoid w', MonadWriter w n m) => MonadWriter w n (StrictRWS.RWST r w' s m) where
  writer n = lift . writer n
  tell   n = lift . tell n
  listen n = liftListenRWS' $ listen n
  pass n   = liftPassRWS' $ pass n

-- }}}

-- Non-increasing instances {{{

instance MonadWriter w n m => MonadWriter w n (Except.ExceptT e m) where
  writer  n = lift . writer n
  tell    n = lift . tell n
  listen  n = Except.liftListen $ listen n
  pass    n = Except.liftPass $ pass n

instance MonadWriter w n m => MonadWriter w n (Identity.IdentityT m) where
  writer  n = lift . writer n
  tell    n = lift . tell n
  listen  n = Identity.mapIdentityT $ listen n
  pass    n = Identity.mapIdentityT $ pass n

instance MonadWriter w n m => MonadWriter w n (Maybe.MaybeT m) where
  writer  n = lift . writer n
  tell    n = lift . tell n
  listen  n = Maybe.liftListen $ listen n
  pass    n = Maybe.liftPass $ pass n

instance MonadWriter w n m => MonadWriter w n (Reader.ReaderT r m) where
  writer  n = lift . writer n
  tell    n = lift . tell n
  listen  n = Reader.mapReaderT $ listen n
  pass    n = Reader.mapReaderT $ pass n

instance MonadWriter w n m => MonadWriter w n (Lazy.StateT s m) where
  writer  n = lift . writer n
  tell    n = lift . tell n
  listen  n = Lazy.liftListen $ listen n
  pass    n = Lazy.liftPass $ pass n

instance MonadWriter w n m => MonadWriter w n (Strict.StateT s m) where
  writer  n = lift . writer n
  tell    n = lift . tell n
  listen  n = Strict.liftListen $ listen n
  pass    n = Strict.liftPass $ pass n

instance (Monoid w, MonadWriter w n m) => MonadWriter w n (Lazy.WriterT w m) where
  writer  n = lift . writer n
  tell    n = lift . tell n
  listen  n = liftListen $ listen n
  pass    n = liftPass $ pass n

instance (Monoid w, MonadWriter w n m) => MonadWriter w n (Strict.WriterT w m) where
  writer  n = lift . writer n
  tell    n = lift . tell n
  listen  n = liftListen' $ listen n
  pass    n = liftPass' $ pass n

-- }}}

-- NoWriter instances {{{

class    NoWriter (m :: * -> *)

instance (NoWriter f, NoWriter g) => NoWriter (Compose f g)
instance                             NoWriter (Constant r)
instance                             NoWriter  Identity
instance (NoWriter f, NoWriter g) => NoWriter (Product f g)
instance  NoWriter f              => NoWriter (Reverse f)
instance (NoWriter f, NoWriter g) => NoWriter (Sum f g)

instance NoWriter m => NoWriter (Except.ExceptT e m)
instance NoWriter m => NoWriter (Identity.IdentityT m)
instance NoWriter m => NoWriter (Maybe.MaybeT m)
instance NoWriter m => NoWriter (Reader.ReaderT r m)
instance NoWriter m => NoWriter (Lazy.StateT s m)
instance NoWriter m => NoWriter (Strict.StateT s m)

-- }}}

-- Lifted Listen/Pass {{{

liftListen :: Monad m => Listen w m (a,w') -> Listen w (Lazy.WriterT w' m) a
liftListen listen m = Lazy.WriterT $ do
  ~((a,w'),w) <- listen $ Lazy.runWriterT m
  return ((a,w),w')

liftPass :: Monad m => Pass w m (a,w') -> Pass w (Lazy.WriterT w' m) a
liftPass pass m = Lazy.WriterT $ pass $ do
  ~((a,f),w) <- Lazy.runWriterT m
  return ((a,w),f)

liftListen' :: Monad m => Listen w m (a,w') -> Listen w (Strict.WriterT w' m) a
liftListen' listen m = Strict.WriterT $ do
  ~((a,w'),w) <- listen $ Strict.runWriterT m
  return ((a,w),w')

liftPass' :: Monad m => Pass w m (a,w') -> Pass w (Strict.WriterT w' m) a
liftPass' pass m = Strict.WriterT $ pass $ do
  ~((a,f),w) <- Strict.runWriterT m
  return ((a,w),f)

liftListenRWS :: Monad m => Listen w m (a,s,w') -> Listen w (LazyRWS.RWST r w' s m) a
liftListenRWS listen m = LazyRWS.RWST $ \r s -> do
  ~((a,s',w),w') <- listen $ LazyRWS.runRWST m r s
  return ((a,w'),s',w)

liftPassRWS :: Monad m => Pass w m (a,s,w') -> Pass w (LazyRWS.RWST r w' s m) a
liftPassRWS pass m = LazyRWS.RWST $ \r s -> pass $ do
  ~((a,f),s',w) <- LazyRWS.runRWST m r s
  return ((a,s',w),f)

liftListenRWS' :: Monad m => Listen w m (a,s,w') -> Listen w (StrictRWS.RWST r w' s m) a
liftListenRWS' listen m = StrictRWS.RWST $ \r s -> do
  ~((a,s',w),w') <- listen $ StrictRWS.runRWST m r s
  return ((a,w'),s',w)

liftPassRWS' :: Monad m => Pass w m (a,s,w') -> Pass w (StrictRWS.RWST r w' s m) a
liftPassRWS' pass m = StrictRWS.RWST $ \r s -> pass $ do
  ~((a,f),s',w) <- StrictRWS.runRWST m r s
  return ((a,s',w),f)

-- }}}

