{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Stream where

import Control.Applicative
import Control.Monad
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Traversable (Traversable)
import qualified Data.Traversable as T
import Data.Maybe
import Data.Monoid
import Control.Monad.Trans.Maybe
import Data.Functor.Classes

newtype Stream' a = Stream'
  { runStream :: MaybeT [] a
  } deriving
  ( Eq , Eq1
  , Ord , Ord1
  , Functor , Foldable , Traversable
  , Applicative
  , Monad
  )

toList :: Stream' a -> [Maybe a]
toList = runMaybeT . runStream

fromList :: [Maybe a] -> Stream' a
fromList = Stream' . MaybeT

onList :: ([Maybe a] -> [Maybe b]) -> Stream' a -> Stream' b
onList f = fromList . f . onList

instance Monoid (Stream' a) where
  mempty = fromList mempty
  mappend (toList -> s1) (toList -> s2) = fromList
    $ case s1 of
      []            -> s2
      Nothing : s1' -> Nothing : (s2  `mappend` s1')
      Just a  : s1' -> Just a  : (s1' `mappend` s2 )

instance Alternative Stream' where
  empty = mempty
  (<|>) = mappend

instance MonadPlus Stream' where
  mzero = mempty
  mplus = mappend

takeStream' :: Int -> Stream' a -> Stream' a
takeStream' i = fromList . map return . take i . catMaybes . toList

data Stream a
  = MT
  | a :* Stream a
  -- | a :* Stream a
  | Delay (Stream a)
  deriving (Eq,Functor,Foldable,Traversable)
infixr 4 :*

instance Show a => Show (Stream a) where
  show s = case s of
    MT       -> ""
    a :* s'  -> show a ++ "\n" ++ show s'
    Delay s' -> unlines $ map ("  " ++) $ lines $ show s'

instance Monoid (Stream a) where
  mempty  = MT
  mappend s1 s2 = case s1 of
    MT       -> s2
    a :* s'  -> a :* (s' `mappend` s2)
    Delay s' -> Delay $ s2 `mappend` s'

instance Applicative Stream where
  pure = return
  (<*>) = ap

instance Monad Stream where
  return = (:* MT)
  s >>= f = case s of
    MT       -> mzero
    a :* s'  -> f a `mplus` (s' >>= f)
    Delay s' -> Delay $ s' >>= f

instance MonadPlus Stream where
  mzero = mempty
  mplus = mappend

instance Alternative Stream where
  empty = mempty
  (<|>) = mappend

takeStream :: Int -> Stream a -> Stream a
takeStream i s
  | i <= 0    = MT
  | otherwise = case s of
    MT       -> MT
    a :* s'  -> ((:*) $! a) $! takeStream (i-1) s'
    Delay s' -> takeStream i s'

