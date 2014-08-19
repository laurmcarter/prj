{-# LANGUAGE DeriveFunctor,DeriveFoldable,DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module UnifyTerm.Goal where

import Control.Applicative
  ( Applicative (..)
  , (<$>)
  )
import Control.Monad.State
  ( MonadPlus (..)
  , MonadState(..)
  , evalStateT
  , StateT (..)
  , ap
  )
import Control.Monad.Reader
  ( MonadReader (..)
  , ReaderT (..)
  , asks
  )

import qualified Data.Foldable as F
import qualified Data.Traversable as T

-- Interleave {{{

newtype Interleave a = Interleave
  { runInterleave :: [a]
  } deriving (Show,Functor,F.Foldable,T.Traversable)

instance Monad Interleave where
  return a = Interleave [a]
  m >>= f  = case runInterleave m of
    []    -> mzero
    a:as' -> f a `mplus` (Interleave as' >>= f)

instance MonadPlus Interleave where
  mzero                   = Interleave []
  (Interleave as) `mplus` m2 = case as of
    []    -> m2
    a:as' -> Interleave (a : runInterleave rest)
      where rest = m2 `mplus` (Interleave as')

-- }}}

-- Streams {{{

type Stream = EitherT String Interleave
runStream :: Stream a -> Interleave (Either String a)
runStream = runEitherT

streamToList :: Stream a -> [Either String a]
streamToList = runInterleave . runStream

newtype EitherT e m a = EitherT
  { runEitherT :: m (Either e a)
  } deriving Functor

instance Monad m => Monad (EitherT String m) where
  return = EitherT . return . Right
  m >>= f = EitherT $ do
    ma <- runEitherT m
    case ma of
      Left e -> return $ Left e
      Right a -> runEitherT $ f a
  fail = EitherT . return . Left

instance MonadPlus m => MonadPlus (EitherT String m) where
  mzero = EitherT mzero
  (EitherT m1) `mplus` (EitherT m2) = EitherT (m1 `mplus` m2)

fromStream :: Maybe Int -> Maybe Int -> Stream a -> Either [String] [a]
fromStream x y = fS x y . runInterleave . runEitherT
  where
    fS en rn as = case (en,rn,as) of
      (Just 0,_,_)            -> Left []  -- convention here?
      (_,Just 0,_)            -> Right [] -- if we need zero more results, then we've succeeded, trivially.
      (_,_,[])                -> Left []  -- if we're out of results, then we've failed.
      (_,_,e@(Left _) : as')  -> orEither e (fS (pr en) rn as')
      (_,_,e@(Right _) : as') -> orEither e (fS en (pr rn) as')
    pr = fmap pred

orEither :: Either a b -> Either [a] [b] -> Either [a] [b]
orEither x re = case (x,re) of
  (Left e,Left es)   -> Left (e:es)
  (Left _,Right rs)  -> Right rs
  (Right r,Left _)  -> Right [r]
  (Right r,Right rs) -> Right (r:rs)

-- }}}

-- Static {{{

evalStatic :: S s a -> s -> Either String a
evalStatic = runReaderT . runS

newtype S s a = S { runS :: ReaderT s (Either String) a } deriving (Functor)

instance Monad (S s) where
  return a = S $ return a
  (S m) >>= f = S $ (m >>= runS . f)

  ---- Branch trimming failure
  --fail err = G mzero

  -- Message collecting failure
  fail = S . ReaderT . const . Left

instance MonadPlus (S s) where
  mzero = S $ mzero
  (S m1) `mplus` (S m2) = S (m1 `mplus` m2)

instance MonadReader s (S s) where
  ask = S ask
  local f = S . local f . runS
  reader = S . reader

instance Applicative (S s) where
  pure = return
  (<*>) = ap

-- }}}

-- Goal {{{

evalGoal :: G s a -> s -> Stream a
evalGoal = evalStateT . runG

runGoal :: G s a -> s -> Stream (a,s)
runGoal = runStateT . runG

newtype G s a = G { runG :: StateT s Stream a } deriving (Functor)

instance Monad (G s) where
  return a = G $ return a
  (G m) >>= f = G $ (m >>= runG . f)

  ---- Branch trimming failure
  --fail err = G mzero

  -- Message collecting failure
  fail err = G $ StateT $ const $ EitherT $ Interleave [Left err]

instance MonadPlus (G s) where
  mzero = G $ mzero
  (G m1) `mplus` (G m2) = G (m1 `mplus` m2)

instance MonadState s (G s) where
  get = G get
  put = G . put

instance Applicative (G s) where
  pure = return
  (<*>) = ap

-- }}}

