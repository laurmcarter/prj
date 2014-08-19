{-# LANGUAGE PolyKinds #-}

module Data.Functor.Show where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity
import Data.List (intersperse)

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Data.Functor.Classes.Extra

class ShowF t where
  showsPrecT :: Monad m => t a -> ShowT_ m
  showT      :: Monad m => t a -> m String
  showT      = flip (execShowT 0) "" . showsPrecT
  showListT  :: Monad m => [Some t] -> ShowT_ m
  showListT ts = do
    showCharT '['
    sequence_
      $ intersperse ( showStringT ", " )
      $ map showsSomeT ts
    showCharT ']'

-- Class function variants {{{

showsPrecM :: ShowF t => t a -> ShowM_
showsPrecM = showsPrecT

showM :: ShowF t => t a -> String
showM = runIdentity . showT

showListM :: ShowF t => [Some t] -> ShowM_
showListM = showListT

showListT' :: (ShowF t, Monad m) => [t a] -> ShowT_ m
showListT' = showListT . map Some

showListM' :: ShowF t => [t a] -> ShowM_
showListM' = showListT'

showsT :: (ShowF t, Monad m) => t a -> ShowT_ m
showsT = withPrec 0 . showsPrecT

showsM :: ShowF t => t a -> ShowM_
showsM = showsT

showsSomePrecT :: (ShowF t, Monad m) => Some t -> ShowT_ m
showsSomePrecT (Some ta) = showsPrecT ta

showsSomePrecM :: ShowF t => Some t -> ShowM_
showsSomePrecM = showsSomePrecT

showsSomeT :: (ShowF t, Monad m) => Some t -> ShowT_ m
showsSomeT (Some ta) = showsT ta

showsSomeM :: ShowF t => Some t -> ShowM_
showsSomeM = showsSomeT

showSomeM :: ShowF t => Some t -> String
showSomeM (Some ta) = showM ta

-- }}}

-- ShowT et al + instances {{{

newtype ShowT m a = ShowT
  { unShowT :: ReaderT Int (StateT String m) a
  }

type ShowT_ m = ShowT m ()
type ShowM    = ShowT Identity
type ShowM_   = ShowM   ()

instance Functor m => Functor (ShowT m) where
  fmap = onReaderShowT . fmap

instance (Functor m, Monad m) => Applicative (ShowT m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (ShowT m) where
  return  = ShowT . return
  m >>= f = ShowT $ unShowT m >>= unShowT . f

instance MonadIO m => MonadIO (ShowT m) where
  liftIO = ShowT . liftIO

-- }}}

-- Run {{{

runShowT :: Int -> ShowT m a -> String -> m (a,String)
runShowT d m inp = flip runStateT inp $ flip runReaderT d $ unShowT m

execShowT :: Monad m => Int -> ShowT m a -> String -> m String
execShowT d m inp = flip execStateT inp $ flip runReaderT d $ unShowT m

runShowM :: Int -> ShowM a -> String -> (a,String)
runShowM d m = runIdentity . runShowT d m

execShowM :: Int -> ShowM a -> String -> String
execShowM d m = runIdentity . execShowT d m

-- }}}

-- Inner Monad Ops {{{

onReaderShowT ::
   ( ReaderT Int (StateT String m) a
  -> ReaderT Int (StateT String n) b
   ) -> ShowT m a -> ShowT n b
onReaderShowT f = ShowT . f . unShowT

onStateShowT :: (StateT String m a -> StateT String n b)
  -> ShowT m a -> ShowT n b
onStateShowT f = onReaderShowT $ ReaderT . fmap f . runReaderT

readerShowT :: Monad m => ReaderT Int (StateT String m) a -> ShowT m a
readerShowT = ShowT

stateShowT :: Monad m => StateT String m a -> ShowT m a
stateShowT = readerShowT . lift

baseShowT :: Monad m => m a -> ShowT m a
baseShowT = stateShowT . lift

askShowT :: Monad m => ShowT m Int
askShowT = readerShowT ask

localShowT :: Monad m => (Int -> Int) -> ShowT m a -> ShowT m a
localShowT = onReaderShowT . local

getShowT :: Monad m => ShowT m String
getShowT = stateShowT get

putShowT :: Monad m => String -> ShowT_ m
putShowT = stateShowT . put

modifyShowT :: Monad m => ShowS -> ShowT_ m
modifyShowT = stateShowT . modify

-- }}}

-- Convert to/from ShowS {{{

showS_to_T :: Monad m => (Int -> ShowS) -> ShowT_ m
showS_to_T f = modifyShowT . f =<< prec

showS_to_M :: (Int -> ShowS) -> ShowM_
showS_to_M = showS_to_T

showT_to_S :: Monad m => (m String -> String) -> ShowT m a -> Int -> ShowS
showT_to_S f m d inp = f $ execShowT d m inp

showM_to_S :: ShowM a -> Int -> ShowS
showM_to_S = showT_to_S runIdentity

-- }}}

-- Additional Ops {{{

prec :: Monad m => ShowT m Int
prec = askShowT

onPrec :: Monad m => (Int -> Int) -> ShowT m a -> ShowT m a
onPrec = localShowT

withPrec :: Monad m => Int -> ShowT m a -> ShowT m a
withPrec = onPrec . const

showStringT :: Monad m => String -> ShowT_ m
showStringT = modifyShowT . flip (++)

showCharT :: Monad m => Char -> ShowT_ m
showCharT = showStringT . (:[])

showParenT :: Monad m => Int -> ShowT m a -> ShowT m a
showParenT d' m = do
  d <- prec
  withPrec (d' + 1)
    $ if (d > d')
      then do
        showCharT '('
        a <- m
        showCharT ')'
        return a
      else m

showStringM :: String -> ShowM_
showStringM = showStringT

showCharM :: Char -> ShowM_
showCharM = showCharT

showParenM :: Int -> ShowM a -> ShowM a
showParenM = showParenT

-- }}}

