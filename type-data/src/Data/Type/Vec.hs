{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Data.Type.Vec where

import Data.Type.Nat as Nat

import Prelude hiding ((++),reverse,length)
import Control.Applicative
import Control.Monad

data Vec (n :: N) (a :: *) where
  Nil  :: Vec Z a
  Cons :: a -> Vec n a -> Vec (S n) a

-- Append {{{

(++) :: Vec m a -> Vec n a -> Vec (m + n) a
as ++ bs = case as of
  Nil        -> bs
  Cons a as' -> Cons a $ as' ++ bs

-- }}}

-- Snoc {{{

snoc :: Vec n a -> a -> Vec (S n) a
snoc as a = case as of
  Nil        -> Cons a Nil
  Cons b as' -> Cons b $ snoc as' a

-- }}}

-- Reverse {{{

reverse :: Vec n a -> Vec n a
reverse as = case as of
  Nil        -> Nil
  Cons a as' -> snoc (reverse as') a

-- }}}

-- Length {{{

length :: Vec n a -> Nat n
length as = case as of
  Nil        -> Z_
  Cons _ as' -> S_ $ length as'

-- }}}

instance Functor (Vec n) where
  fmap f as = case as of
    Nil        -> Nil
    Cons a as' -> Cons (f a) $ fmap f as'

-- Index {{{

{-
_Index :: forall f x n n' a. (Functor f, x < n) => Nat x -> (a -> f a) -> Vec n a -> f (Vec n a)
_Index x f as = case (leEvidence :: Le (S x) n) of
  Lt (LeZ y) -> _
-}

-- }}}

