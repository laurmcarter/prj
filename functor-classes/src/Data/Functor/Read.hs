{-# LANGUAGE PolyKinds #-}

module Data.Functor.Read where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity
import Control.Monad.Trans.List

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Data.Functor.Classes.Extra

class ReadF t where
  readsPrecT :: Monad m => ReadT m (Some t)
  readListT  :: Monad m => ReadT m [Some t]

-- Class function variants {{{

readsPrecM :: ReadF t => ReadM (Some t)
readsPrecM = readsPrecT

readsT :: (ReadF t, Monad m) => ReadT m (Some t)
readsT = withPrec 0 $ readsPrecT

readsM :: ReadF t => ReadM (Some t)
readsM = readsT

readT :: (ReadF t, MonadPlus m) => String -> m (Some t)
readT = evalReadT 0 readsPrecT >=> go
  where
  go as = case as of
    []    -> mzero
    a:as' -> return a `mplus` go as'

readM :: ReadF t => String -> Maybe (Some t)
readM = readT

-- }}}

-- ReadT et al + instances {{{

newtype ReadT m a = ReadT
  { unReadT :: ReaderT Int (StateT String (ListT m)) a
  }

type ReadT_ m = ReadT m ()
type ReadM    = ReadT Identity
type ReadM_   = ReadM   ()

instance Functor m => Functor (ReadT m) where
  fmap f (ReadT m) = ReadT $ fmap f m

instance (Functor m, Monad m) => Applicative (ReadT m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (ReadT m) where
  return  = ReadT . return
  m >>= f = ReadT $ unReadT m >>= unReadT . f

instance (Applicative m, Monad m) => Alternative (ReadT m) where
  empty   = ReadT empty
  x <|> y = ReadT $ unReadT x <|> unReadT y

instance Monad m => MonadPlus (ReadT m) where
  mzero     = ReadT mzero
  mplus x y = ReadT $ unReadT x `mplus` unReadT y

instance MonadIO m => MonadIO (ReadT m) where
  liftIO = ReadT . liftIO

-- }}}

-- Run {{{

runReadT :: Int -> ReadT m a -> String -> m [(a,String)]
runReadT d m inp =
    runListT
  $ flip runStateT inp
  $ flip runReaderT d
  $ unReadT m

evalReadT :: Monad m => Int -> ReadT m a -> String -> m [a]
evalReadT d m inp =
    runListT
  $ flip evalStateT inp
  $ flip runReaderT d
  $ unReadT m

runReadM :: Int -> ReadM a -> String -> [(a,String)]
runReadM d m = runIdentity . runReadT d m

evalReadM :: Int -> ReadM a -> String -> [a]
evalReadM d m = runIdentity . evalReadT d m

-- }}}

-- Inner Monad Ops {{{

onReaderReadT ::
   ( ReaderT Int (StateT String (ListT m)) a
  -> ReaderT Int (StateT String (ListT n)) b
   ) -> ReadT m a -> ReadT n b
onReaderReadT f = ReadT . f . unReadT

onStateReadT :: (StateT String (ListT m) a -> StateT String (ListT n) b)
  -> ReadT m a -> ReadT n b
onStateReadT f = onReaderReadT $ ReaderT . fmap f . runReaderT

onListReadT :: (Functor m, Functor n) => (ListT m a -> ListT n b) -> ReadT m a -> ReadT n b
onListReadT f = onStateReadT $ StateT . go . runStateT
  where
  go g inp = fmap (flip (,) inp) $ f $ fmap fst $ g inp

readerReadT :: Monad m => ReaderT Int (StateT String (ListT m)) a -> ReadT m a
readerReadT = ReadT

stateReadT :: Monad m => StateT String (ListT m) a -> ReadT m a
stateReadT = readerReadT . lift

listReadT :: Monad m => ListT m a -> ReadT m a
listReadT = stateReadT . lift

baseReadT :: Monad m => m a -> ReadT m a
baseReadT = listReadT . lift

askReadT :: Monad m => ReadT m Int
askReadT = readerReadT ask

localReadT :: Monad m => (Int -> Int) -> ReadT m a -> ReadT m a
localReadT = onReaderReadT . local

getReadT :: Monad m => ReadT m String
getReadT = stateReadT get

putReadT :: Monad m => String -> ReadT_ m
putReadT = stateReadT . put

modifyReadT :: Monad m => (String -> String) -> ReadT_ m
modifyReadT = stateReadT . modify

-- }}}

-- Convert to/from ReadS {{{

readS_to_T :: Monad m => (Int -> ReadS a) -> ReadT m a
readS_to_T f = ReadT $
  ReaderT $ \d ->
  StateT $ \inp ->
  ListT $ return $ f d inp

readS_to_M :: (Int -> ReadS a) -> ReadM a
readS_to_M = readS_to_T

readT_to_S :: (m [(a,String)] -> [(a,String)]) -> ReadT m a -> Int -> ReadS a
readT_to_S f m d inp = f $ runReadT d m inp

readM_to_S :: ReadM a -> Int -> ReadS a
readM_to_S = readT_to_S runIdentity

-- }}}

-- Additional Ops {{{

prec :: Monad m => ReadT m Int
prec = askReadT

onPrec :: Monad m => (Int -> Int) -> ReadT m a -> ReadT m a
onPrec = localReadT

withPrec :: Monad m => Int -> ReadT m a -> ReadT m a
withPrec = onPrec . const

inputT :: Monad m => ReadT m String
inputT = getReadT

outputT :: Monad m => String -> ReadT_ m
outputT = putReadT

readCharT :: Monad m => ReadT m Char
readCharT = inputT >>= \inp -> case inp of
  c:inp' -> putReadT inp' >> return c
  _      -> mzero

readParenT :: Monad m => Bool -> ReadT m a -> ReadT m a
readParenT b m = if b then mand else optl
  where
  mand = do
    "(" <- lexT
    a   <- optl
    ")" <- lexT
    return a
  optl = m `mplus` mand

lexT :: Monad m => ReadT m String
lexT = readS_to_T $ const lex

endT :: Monad m => ReadT_ m
endT = guard . null =<< inputT

-- }}}

