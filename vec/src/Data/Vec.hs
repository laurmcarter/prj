{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Data.Vec where

import Control.Applicative
import Data.Nat
import Data.Nat.Proxies

import Data.Proxy
import Data.Monoid
import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Traversable (Traversable)
import qualified Data.Traversable as T

data Vec (n :: Nat) :: * -> * where
  Nil  :: Vec Z a
  (:*) :: Witness n NatW => a -> Vec n a -> Vec (S n) a

infixr 4 :*

-- Eq Show {{{

instance Eq a => Eq (Vec n a) where
  Nil == Nil = True
  (a :* u) == (b :* v) = a == b && u == v
  _ == _ = False

instance Show a => Show (Vec n a) where
  showsPrec d v = showParen (d > 5)
    $ showString "fromList "
    . showList (toList v)

-- }}}

-- Functor Foldable Traversable {{{

instance Functor (Vec n) where
  fmap f vec = case vec of
    Nil    -> Nil
    a :* v -> f a :* fmap f v

instance Foldable (Vec n) where
  foldMap f vec = case vec of
    Nil    -> mempty
    a :* v -> f a <> F.foldMap f v

instance Traversable (Vec n) where
  traverse f vec = case vec of
    Nil    -> pure Nil
    a :* v -> (:*) <$> f a <*> T.traverse f v

instance Witness n NatW => Applicative (Vec n) where
  pure a = iterateNat Nil (a :*) $ witness (Proxy :: Proxy n)
  fs <*> xs = case (fs,xs) of
    (Nil,Nil) -> Nil
    (f :* fs',x :* xs') -> f x :* (fs' <*> xs')
    _ -> error "Never happens"

-- }}}

toList :: Vec n a -> [a]
toList vec = case vec of
  Nil -> []
  a :* v -> a : toList v

(*:) :: a -> a -> Vec Two a
a *: b = a :* b :* Nil

append :: Vec m a -> Vec n a -> Vec (Add m n) a
append v1 v2 = case v1 of
  Nil     -> v2
  a :* v' -> a :* append v' v2

flatten :: Vec m (Vec n a) -> Vec (Mul m n) a
flatten vecs = case vecs of
  Nil -> Nil
  v :* vs -> append v (flatten vs)

combination :: Vec n a -> NatW k -> Vec (Choose n k) (Vec k a)
combination v k = case (v,k) of
  (_      ,ZW)    -> Nil :* Nil
  (Nil    ,SW _)  -> Nil
  (a :* v',SW k') -> append
    (combination v' k)
    ((a :*) <$> combination v' k')

