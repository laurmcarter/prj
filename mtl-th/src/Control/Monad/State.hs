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

module Control.Monad.State
  ( module Control.Monad.State
  , module Control.Monad.Trans.State
  ) where

import Control.Monad.Trans.State
  ( StateT(..) , mapStateT , withStateT
  , State      , runState  , mapState , withState
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


class Monad m => MonadStateN n m where
  getN :: proxy n -> m (StateAt n m)
  --
  default getN :: (MonadTrans t, MonadStateAt n s m)
    => proxy n -> t m s
  getN n = lift $ getN n
  ----
  putN :: proxy n -> StateAt n m -> m ()
  --
  default putN :: (MonadTrans t, MonadStateAt n s m)
    => proxy n -> s -> t m ()
  putN n = lift . putN n
  ----
  stateN :: proxy n -> (StateAt n m -> (a,StateAt n m)) -> m a
  --
  default stateN :: (MonadTrans t, MonadStateAt n s m)
    => proxy n -> (s -> (a,s)) -> t m a
  stateN n = lift . stateN n

type MonadStateAt n s m = (MonadStateN n m, s ~ StateAt n m)
type MonadState'    s m = MonadStateAt Z s m

type MonadState m = MonadStateN Z m
type StateOf    m = StateAt Z m


-- Convenience {{{

get :: MonadState' s m => m s
get = getN Z_

gets :: MonadState' s m => (s -> a) -> m a
gets = getsN Z_

put :: MonadState' s m => s -> m ()
put = putN Z_

state :: MonadState' s m => (s -> (a,s)) -> m a
state = stateN Z_

modify :: MonadState' s m => (s -> s) -> m ()
modify = modifyN Z_


get1 :: MonadStateAt (S Z) s m => m s
get1 = getN $ S_ Z_

gets1 :: MonadStateAt (S Z) s m => (s -> a) -> m a
gets1 = getsN $ S_ Z_

put1 :: MonadStateAt (S Z) s m => s -> m ()
put1 = putN $ S_ Z_

state1 :: MonadStateAt (S Z) s m => (s -> (a,s)) -> m a
state1 = stateN $ S_ Z_

modify1 :: MonadStateAt (S Z) s m => (s -> s) -> m ()
modify1 = modifyN $ S_ Z_


getsN :: MonadStateAt n s m => proxy n -> (s -> a) -> m a
getsN n f = liftM f $ getN n

modifyN :: MonadStateAt n s m => proxy n -> (s -> s) -> m ()
modifyN n f = getN n >>= putN n . f

-- }}}


-- State Instances {{{

instance Monad m => MonadStateN Z (Lazy.StateT s m) where
  getN   _ = Lazy.get
  putN   _ = Lazy.put
  stateN _ = Lazy.state
instance MonadStateN n m => MonadStateN (S n) (Lazy.StateT s m) where
  getN   _ = lift $ getN (Proxy :: Proxy n)
  putN   _ = lift . putN (Proxy :: Proxy n)
  stateN _ = lift . stateN (Proxy :: Proxy n)

instance Monad m => MonadStateN Z (Strict.StateT s m) where
  getN   _ = Strict.get
  putN   _ = Strict.put
  stateN _ = Strict.state
instance MonadStateN n m => MonadStateN (S n) (Strict.StateT s m) where
  getN   _ = lift $ getN (Proxy :: Proxy n)
  putN   _ = lift . putN (Proxy :: Proxy n)
  stateN _ = lift . stateN (Proxy :: Proxy n)

instance (Monoid w, Monad m) => MonadStateN Z (LazyRWS.RWST r w s m) where
  getN   _ = LazyRWS.get
  putN   _ = LazyRWS.put
  stateN _ = LazyRWS.state
instance (Monoid w, MonadStateN n m) => MonadStateN (S n) (LazyRWS.RWST r w s m) where
  getN   _ = lift $ getN (Proxy :: Proxy n)
  putN   _ = lift . putN (Proxy :: Proxy n)
  stateN _ = lift . stateN (Proxy :: Proxy n)

instance (Monoid w, Monad m) => MonadStateN Z (StrictRWS.RWST r w s m) where
  getN   _ = StrictRWS.get
  putN   _ = StrictRWS.put
  stateN _ = StrictRWS.state
instance (Monoid w, MonadStateN n m) => MonadStateN (S n) (StrictRWS.RWST r w s m) where
  getN   _ = lift $ getN (Proxy :: Proxy n)
  putN   _ = lift . putN (Proxy :: Proxy n)
  stateN _ = lift . stateN (Proxy :: Proxy n)

-- }}}


-- Non-State Instances {{{

instance            MonadStateN n m  => MonadStateN n (Cont.ContT r m)
instance            MonadStateN n m  => MonadStateN n (Except.ExceptT e m)
instance            MonadStateN n m  => MonadStateN n (Identity.IdentityT m)
instance            MonadStateN n m  => MonadStateN n (List.ListT m)
instance            MonadStateN n m  => MonadStateN n (Maybe.MaybeT m)
instance            MonadStateN n m  => MonadStateN n (Reader.ReaderT s m)
instance (Monoid w, MonadStateN n m) => MonadStateN n (Lazy.WriterT w m)
instance (Monoid w, MonadStateN n m) => MonadStateN n (Strict.WriterT w m)

-- }}}


-- StateAt Type Instances {{{

type family StateAt (n :: N) (m :: * -> *) :: *

type instance StateAt Z        (Lazy.StateT s m) = s
type instance StateAt Z      (Strict.StateT s m) = s
type instance StateAt Z   (LazyRWS.RWST r w s m) = s
type instance StateAt Z (StrictRWS.RWST r w s m) = s

type instance StateAt (S n)        (Lazy.StateT s m) = StateAt n m
type instance StateAt (S n)      (Strict.StateT s m) = StateAt n m
type instance StateAt (S n)   (LazyRWS.RWST r w s m) = StateAt n m
type instance StateAt (S n) (StrictRWS.RWST r w s m) = StateAt n m

type instance StateAt n       (Cont.ContT s m) = StateAt n m
type instance StateAt n   (Except.ExceptT e m) = StateAt n m
type instance StateAt n (Identity.IdentityT m) = StateAt n m
type instance StateAt n         (List.ListT m) = StateAt n m
type instance StateAt n       (Maybe.MaybeT m) = StateAt n m
type instance StateAt n   (Reader.ReaderT r m) = StateAt n m
type instance StateAt n     (Lazy.WriterT w m) = StateAt n m
type instance StateAt n   (Strict.WriterT w m) = StateAt n m

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

