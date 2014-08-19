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

module Control.Monad.Cont
  ( module Control.Monad.Cont
  , module Control.Monad.Trans.Cont
  ) where

import Control.Monad.Trans.Cont
  ( ContT(..) , mapContT , withContT
  , Cont      , runCont  , mapCont , withCont
  )
import Control.Monad
import Control.Monad.Morph
import Control.Monad.Signatures

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


class Monad m => MonadContN (n :: N) m where
  callCCN :: proxy n -> ((a -> m b) -> m a) -> m a

type MonadCont  m = MonadContN Z m


{-
-- Convenience {{{

get :: MonadCont' s m => m s
get = getN Z_

gets :: MonadCont' s m => (s -> a) -> m a
gets = getsN Z_

put :: MonadCont' s m => s -> m ()
put = putN Z_

state :: MonadCont' s m => (s -> (a,s)) -> m a
state = stateN Z_

modify :: MonadCont' s m => (s -> s) -> m ()
modify = modifyN Z_


get1 :: MonadContAt (S Z) s m => m s
get1 = getN $ S_ Z_

gets1 :: MonadContAt (S Z) s m => (s -> a) -> m a
gets1 = getsN $ S_ Z_

put1 :: MonadContAt (S Z) s m => s -> m ()
put1 = putN $ S_ Z_

state1 :: MonadContAt (S Z) s m => (s -> (a,s)) -> m a
state1 = stateN $ S_ Z_

modify1 :: MonadContAt (S Z) s m => (s -> s) -> m ()
modify1 = modifyN $ S_ Z_


getsN :: MonadContAt n s m => proxy n -> (s -> a) -> m a
getsN n f = liftM f $ getN n

modifyN :: MonadContAt n s m => proxy n -> (s -> s) -> m ()
modifyN n f = getN n >>= putN n . f

-- }}}
-}


-- Cont Instances {{{

instance Monad m => MonadContN Z (ContT r m) where
  callCCN _ = Cont.callCC

instance MonadContN n m => MonadContN (S n) (ContT r m) where
  callCCN n = undefined -- lift . callCCN n

-- }}}


{-
-- Non-Cont Instances {{{

instance            MonadContN n m  => MonadContN n (Except.ExceptT e m)
instance            MonadContN n m  => MonadContN n (Identity.IdentityT m)
instance            MonadContN n m  => MonadContN n (List.ListT m)
instance            MonadContN n m  => MonadContN n (Maybe.MaybeT m)
instance            MonadContN n m  => MonadContN n (Reader.ReaderT s m)
instance            MonadContN n m  => MonadContN n (LazyRWS.RWST r w s m)
instance            MonadContN n m  => MonadContN n (StrictRWS.RWST r w s m)
instance            MonadContN n m  => MonadContN n (Lazy.StateT s m)
instance            MonadContN n m  => MonadContN n (Strict.StateT s m)
instance (Monoid w, MonadContN n m) => MonadContN n (Lazy.WriterT w m)
instance (Monoid w, MonadContN n m) => MonadContN n (Strict.WriterT w m)

-- }}}
-}


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

