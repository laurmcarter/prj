{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Unify.MKTypes where

import Unify

import Control.Applicative
import qualified Data.Foldable as F
import qualified Data.Traversable as T

data Cons a = Cons a a deriving (Show,Functor,F.Foldable,T.Traversable)

instance Render Cons where
  render (Cons a d) = "(" ++ show a ++ " . " ++ show d ++ ")"

instance Equal Cons where
  equal (Cons a1 d1) (Cons a2 d2) = a1 == a2 && d1 == d2

instance (LogicVar v) => Uni Cons v where
  uni (Cons a1 d1) (Cons a2 d2) = unify a1 a2 >> unify d1 d2

int :: (Const Int :<: f) => Int -> Mu f
int = inject . Const

bool :: (Const Bool :<: f) => Bool -> Mu f
bool = inject . Const

char :: (Const Char :<: f) => Char -> Mu f
char = inject . Const

str :: (Const String :<: f) => String -> Mu f
str = inject . Const

nil :: (Const () :<: f) => Mu f
nil = inject $ Const ()

cons :: (Cons :<: f) => Mu f -> Mu f -> Mu f
cons a d = inject $ Cons a d

list :: (Cons :<: f, Const () :<: f) => [Mu f] -> Mu f
list xs = case xs of
  []    -> nil
  x:xs' -> cons x $ list xs'

