{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Type.Nat where

import Type.Equality
import Type.Witness
import Type.Value

data N = Z | S N
  deriving (Eq,Show)

data Nat (x :: N) where
  NZ :: Nat Z
  NS :: Nat x -> Nat (S x)

type IsNat n = (WitBy n Nat,Reps n Int)

-- Witness {{{

instance Witness Z where
  type Wit Z = Nat
  witness = NZ

instance IsNat n => Witness (S n) where
  type Wit (S n) = Nat
  witness = NS witness

-- }}}

-- Value {{{

instance Value Z where
  type Val Z = Int
  toVal NZ = 0

instance IsNat n => Value (S n) where
  type Val (S n) = Int
  toVal (NS n) = 1 + toVal n

-- }}}

-- Succ Pred {{{

type family Pred (x :: N) :: N where
  Pred (S x) = x

suc :: Nat x -> Nat (S x)
suc = NS

prd :: Nat (S x) -> Nat x
prd (NS x) = x

-- }}}

-- Add {{{

type family (:+) (x :: N) (y :: N) :: N where
  Z   :+ y = y
  S x :+ y = S (x :+ y)

(+:) :: Nat x -> Nat y -> Nat (x :+ y)
x +: y = case x of
  NZ    ->            y
  NS x' -> NS $ x' +: y

infixr 7 :+
infixr 7 +:

-- }}}

-- Mul {{{

type family (:*) (x :: N) (y :: N) :: N where
  Z   :* y = Z
  S x :* y = y :+ (x :* y)

(*:) :: Nat x -> Nat y -> Nat (x :* y)
x *: y = case x of
  NZ    -> NZ
  NS x' -> y +: (x' *: y)

infixr 8 :*
infixr 8 *:

-- }}}

