{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.Layered.Except.Class where

import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Cont          as Cont
import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except        as ExceptT
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
import Data.Functor.Compose
import Data.Functor.Constant
import Data.Functor.Identity
import Data.Functor.Product
import Data.Functor.Reverse
import Data.Functor.Sum

import Control.Exception(IOException,catch,ioError)
import Control.Monad
import Data.Monoid hiding (Sum(..),Product(..))

class Monad m => MonadError e (n :: N) m | m n -> e where
  throwError :: proxy n -> e -> m a
  catchError :: proxy n -> m a -> (e -> m a) -> m a

-- Base instances {{{

instance (Monad m, NoError m) => MonadError e Z (ExceptT e m) where
  throwError _ = ExceptT.throwE
  catchError _ = ExceptT.catchE

-- }}}

-- Increasing instances {{{

instance MonadError e' n m => MonadError e (S n) (ExceptT e m) where
  throwError _ = ExceptT.throwE
  catchError _ = ExceptT.catchE

-- }}}

-- Pass-through instances {{{

instance MonadError e n m => MonadError e n (ExceptT e' m) where
  throwError n = lift . throwError n
  catchError n = undefined -- ExceptT.mapExceptT . catchError n

-- }}}

{-
-- Non-increasing instances {{{

instance MonadError s' n m => MonadError s' n (Cont.ContT r m) where
  throwError    n = lift $ throwError n
  catchError    n = lift . catchError n

instance MonadError s n m => MonadError s n (ExceptT e m) where
  throwError    n = lift $ throwError n
  catchError    n = lift . catchError n

instance MonadError s n m => MonadError s n (IdentityT m) where
  throwError    n = lift $ throwError n
  catchError    n = lift . catchError n

instance MonadError s n m => MonadError s n (ListT m) where
  throwError    n = lift $ throwError n
  catchError    n = lift . catchError n

instance MonadError s n m => MonadError s n (MaybeT m) where
  throwError    n = lift $ throwError n
  catchError    n = lift . catchError n

instance MonadError s n m => MonadError s n (Reader.ReaderT r m) where
  throwError    n = lift $ throwError n
  catchError    n = lift . catchError n

instance (Monoid w, MonadError s n m) => MonadError s n (Lazy.WriterT w m) where
  throwError    n = lift $ throwError n
  catchError    n = lift . catchError n

instance (Monoid w, MonadError s n m) => MonadError s n (Strict.WriterT w m) where
  throwError    n = lift $ throwError n
  catchError    n = lift . catchError n

-- }}}
-}

-- NoError instances {{{

class    NoError (m :: * -> *)

instance (NoError f, NoError g) => NoError (Compose f g)
instance                           NoError (Constant r)
instance                           NoError  Identity
instance (NoError f, NoError g) => NoError (Product f g)
instance  NoError f             => NoError (Reverse f)
instance (NoError f, NoError g) => NoError (Sum f g)

instance NoError m => NoError (Cont.ContT r m)
instance NoError m => NoError (ExceptT e m)
instance NoError m => NoError (IdentityT m)
instance NoError m => NoError (ListT m)
instance NoError m => NoError (MaybeT m)
instance NoError m => NoError (Reader.ReaderT r m)
instance NoError m => NoError (Lazy.WriterT w m)
instance NoError m => NoError (Strict.WriterT w m)

-- }}}

