{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Data.Monoid.Free where

import Data.Monoid

data Free (t :: *) where
  Mempty :: Free t
  (:<>:) :: t -> Free t -> Free t
infixr 4 :<>:

instance Monoid (Free t) where
  mempty      = Mempty
  mappend a b = case a of
    Mempty    -> b
    t :<>: a' -> t :<>: mappend a' b

runMonoid :: Monoid r => (t -> r) -> Free t -> r
runMonoid f a = case a of
  Mempty    -> mempty
  t :<>: a' -> f t <> runMonoid f a'

