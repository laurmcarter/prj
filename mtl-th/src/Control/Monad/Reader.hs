{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Reader
  ( module Control.Monad.Reader
  , module Control.Monad.Trans.Reader
  ) where

import Control.Monad.Trans.Reader
  ( ReaderT(..) , mapReaderT , withReaderT
  , Reader      , runReader  , mapReader , withReader
  )
import Control.Monad
import Control.Monad.Morph

import qualified Control.Monad.Trans.Cont          as Cont
import qualified Control.Monad.Trans.Except        as Except
import qualified Control.Monad.Trans.Identity      as Identity
import qualified Control.Monad.Trans.List          as List
import qualified Control.Monad.Trans.Maybe         as Maybe
import qualified Control.Monad.Trans.Reader        as Reader
import qualified Control.Monad.Trans.RWS.Lazy      as LazyRWS
import qualified Control.Monad.Trans.RWS.Strict    as StrictRWS
import qualified Control.Monad.Trans.State.Lazy    as Lazy
import qualified Control.Monad.Trans.State.Strict  as Strict
import qualified Control.Monad.Trans.Writer.Lazy   as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Data.Monoid (Monoid)

import Data.Proxy


class Monad m => MonadReaderN n m where
  askN :: proxy n -> m (ReaderAt n m)
  --
  default askN :: (MonadTrans t, MonadReaderAt n r m)
    => proxy n -> t m r
  askN n = lift $ askN n
  ----
  localN :: proxy n -> (ReaderAt n m -> ReaderAt n m) -> m a -> m a
  --
  default localN:: (MFunctor t, MonadReaderAt n r m)
    => proxy n -> (r -> r) -> t m a -> t m a
  localN n f = hoist $ localN n f
  ----
  readerN :: proxy n -> (ReaderAt n m -> a) -> m a
  --
  default readerN :: (MonadTrans t, MonadReaderAt n r m)
    => proxy n -> (r -> a) -> t m a
  readerN n = lift . readerN n

type MonadReaderAt n r m = (MonadReaderN n m, r ~ ReaderAt n m)
type MonadReader'    r m = MonadReaderAt Z r m

type MonadReader m = MonadReaderN Z m
type ReaderOf    m = ReaderAt Z m


-- Convenience {{{

ask :: MonadReader' r m => m r
ask = askN Z_

asks :: MonadReader' r m => (r -> a) -> m a
asks = asksN Z_

local :: MonadReader' r m => (r -> r) -> m a -> m a
local = localN Z_

reader :: MonadReader' r m => (r -> a) -> m a
reader = readerN Z_


ask1 :: MonadReaderAt (S Z) r m => m r
ask1 = askN $ S_ Z_

asks1 :: MonadReaderAt (S Z) r m => (r -> a) -> m a
asks1 = asksN $ S_ Z_

local1 :: MonadReaderAt (S Z) r m => (r -> r) -> m a -> m a
local1 = localN $ S_ Z_

reader1 :: MonadReaderAt (S Z) r m => (r -> a) -> m a
reader1 = readerN $ S_ Z_


asksN :: MonadReaderAt n r m => proxy n -> (r -> a) -> m a
asksN n f = liftM f $ askN n

-- }}}


-- Reader Instances {{{

instance Monad m => MonadReaderN Z (Reader.ReaderT r m) where
  askN    _ = Reader.ask
  localN  _ = Reader.local
  readerN _ = Reader.reader
instance MonadReaderN n m => MonadReaderN (S n) (Reader.ReaderT r m) where
  askN    _   = lift $ askN (Proxy :: Proxy n)
  localN  _ f = hoist $ localN (Proxy :: Proxy n) f
  readerN _   = lift . readerN (Proxy :: Proxy n)

instance (Monoid w, Monad m) => MonadReaderN Z (LazyRWS.RWST r w s m) where
  askN    _ = LazyRWS.ask
  localN  _ = LazyRWS.local
  readerN _ = LazyRWS.reader
instance (Monoid w, MonadReaderN n m) => MonadReaderN (S n) (LazyRWS.RWST r w s m) where
  askN    _   = lift $ askN (Proxy :: Proxy n)
  localN  _ f = hoist $ localN (Proxy :: Proxy n) f
  readerN _   = lift . readerN (Proxy :: Proxy n)

instance (Monoid w, Monad m) => MonadReaderN Z (StrictRWS.RWST r w s m) where
  askN    _   = StrictRWS.ask
  localN  _   = StrictRWS.local
  readerN _   = StrictRWS.reader
instance (Monoid w, MonadReaderN n m) => MonadReaderN (S n) (StrictRWS.RWST r w s m) where
  askN    _   = lift $ askN (Proxy :: Proxy n)
  localN  _ f = hoist $ localN (Proxy :: Proxy n) f
  readerN _   = lift . readerN (Proxy :: Proxy n)

-- }}}


-- Non-Reader Instances {{{

instance            MonadReaderN n m  => MonadReaderN n (Cont.ContT r m) where
  localN  n = Cont.mapContT . localN n

instance            MonadReaderN n m  => MonadReaderN n (Except.ExceptT e m) where
  localN  n = Except.mapExceptT . localN n

instance            MonadReaderN n m  => MonadReaderN n (Identity.IdentityT m)
instance            MonadReaderN n m  => MonadReaderN n (List.ListT m)
instance            MonadReaderN n m  => MonadReaderN n (Maybe.MaybeT m)
instance            MonadReaderN n m  => MonadReaderN n (Lazy.StateT s m)
instance            MonadReaderN n m  => MonadReaderN n (Strict.StateT s m)
instance (Monoid w, MonadReaderN n m) => MonadReaderN n (Lazy.WriterT w m)
instance (Monoid w, MonadReaderN n m) => MonadReaderN n (Strict.WriterT w m)

-- }}}


-- ReaderAt Type Instances {{{

type family ReaderAt (n :: N) (m :: * -> *) :: *

type instance ReaderAt Z     (Reader.ReaderT r m) = r
type instance ReaderAt Z   (LazyRWS.RWST r w s m) = r
type instance ReaderAt Z (StrictRWS.RWST r w s m) = r

type instance ReaderAt (S n)     (Reader.ReaderT r m) = ReaderAt n m
type instance ReaderAt (S n)   (LazyRWS.RWST r w s m) = ReaderAt n m
type instance ReaderAt (S n) (StrictRWS.RWST r w s m) = ReaderAt n m

type instance ReaderAt n       (Cont.ContT r m) = ReaderAt n m
type instance ReaderAt n   (Except.ExceptT e m) = ReaderAt n m
type instance ReaderAt n (Identity.IdentityT m) = ReaderAt n m
type instance ReaderAt n         (List.ListT m) = ReaderAt n m
type instance ReaderAt n       (Maybe.MaybeT m) = ReaderAt n m
type instance ReaderAt n      (Lazy.StateT s m) = ReaderAt n m
type instance ReaderAt n    (Strict.StateT s m) = ReaderAt n m
type instance ReaderAt n     (Lazy.WriterT w m) = ReaderAt n m
type instance ReaderAt n   (Strict.WriterT w m) = ReaderAt n m

-- }}}


-- Nats {{{

data N
  = Z
  | S N
  deriving (Eq,Show)

data Nat (n :: N) where
  Z_ :: Nat Z
  S_ :: Nat n -> Nat (S n)

zero :: Nat Z
zero = Z_
one :: Nat (S Z)
one = S_ zero
two :: Nat (S (S Z))
two = S_ one
three :: Nat (S (S (S Z)))
three = S_ two

-- }}}

