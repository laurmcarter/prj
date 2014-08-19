{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Data.Nat where

data N
  = Z
  | S N
  deriving (Eq,Show)

data Nat (x :: N) where
  Z_ :: Nat Z
  S_ :: Nat n -> Nat (S n)

type family (x :: N) + (y :: N) :: N where
  Z   + y = y
  S x + y = S (x + y)

(+:) :: Nat x -> Nat y -> Nat (x + y)
x +: y = case x of
  Z_    -> y
  S_ x' -> S_ $ x' +: y

type family (x :: N) * (y :: N) :: N where
  Z   * y = Z
  S x * y = y + (x * y)
infixl 6 +:

(*:) :: Nat x -> Nat y -> Nat (x * y)
x *: y = case x of
  Z_    -> Z_
  S_ x' -> y +: x' *: y
infixl 7 *:

zero :: Nat Z
zero = Z_

one :: Nat (S Z)
one = S_ zero

two :: Nat (S (S Z))
two = S_ one

