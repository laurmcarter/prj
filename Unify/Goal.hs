{-# LANGUAGE DeriveFunctor,DeriveFoldable,DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Unify.Goal where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Monoid
import qualified Data.Foldable as F
import qualified Data.Traversable as T

type Subst t = [(t,t)]

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

newtype EitherT e m a = EitherT
  { runEitherT :: m (Either e a)
  } deriving Functor

instance Monad m => Monad (EitherT e m) where
  return = EitherT . return . Right
  m >>= f = EitherT $ do
    ma <- runEitherT m
    case ma of
      Left e -> return $ Left e
      Right a -> runEitherT $ f a

instance MonadPlus m => MonadPlus (EitherT e m) where
  mzero = EitherT mzero
  (EitherT m1) `mplus` (EitherT m2) = EitherT (m1 `mplus` m2)

fromStream :: Maybe Int -> Maybe Int -> Stream a -> Either [String] [a]
fromStream x y = fS x y . runInterleave . runEitherT
  where
    fS en rn as = case (en,rn,as) of
      (Just 0,_,_)            -> Left []  -- convention here?
      (_,Just 0,_)            -> Right [] -- if we need zero more results, than we've succeeded (trivially)?
      (_,_,[])                -> Left []  -- if we're out of results, then we've failed, trivially.
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

-- Goal {{{

evalGoal :: G s t a -> s -> Stream a
evalGoal m s = evalStateT (runG m) s

newtype G s t a = G { runG :: StateT s Stream a } deriving (Functor)

instance Monad (G s t) where
  return a = G $ return a
  (G m) >>= f = G $ (m >>= runG . f)

  ---- Branch trimming failure
  --fail err = G mzero

  -- Message collecting failure
  fail err = G $ StateT $ const $ EitherT $ Interleave [Left err]

instance MonadPlus (G s t) where
  mzero = G $ mzero
  (G m1) `mplus` (G m2) = G (m1 `mplus` m2)

instance MonadState s (G s t) where
  get = G get
  put = G . put

instance Applicative (G s t) where
  pure = return
  mf <*> mv = do
    f <- mf
    v <- mv
    return (f v)

-- }}}

