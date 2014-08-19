{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Data.Nat where

import Prelude hiding (div,mod,fst,snd)
import Data.Proxy
import Data.Proxy.TH

data N
  = Z
  | S N
  deriving (Eq)

class IsNat (n :: N) where
  witness :: Proxy n -> Nat n
instance IsNat Z where
  witness _ = Nz
instance IsNat n => IsNat (S n) where
  witness _ = Ns $ witness (Proxy :: Proxy n)

-- Show {{{

toInt :: N -> Int
toInt n = case n of
  Z -> 0
  S n' -> succ $ toInt n'

instance Show N where
  showsPrec d n = showParen (d > 5)
    $ showString "N "
    . shows (toInt n)

-- }}}

-- Succ Pred {{{

type family Succ (n :: N) :: N where
  Succ    Z  = S Z
  Succ (S n) = S (S n)

type family Pred (n :: N) :: N where
  Pred (S n) = n

-- }}}

-- Add Sub {{{

type family Add (m :: N) (n :: N) :: N where
  Add    Z  n = n
  Add (S m) n = S (Add m n)

type family Sub (m :: N) (n :: N) :: N where
  Sub     Z     Z = Z
  Sub (S m)     Z = S m
  Sub (S m) (S n) = Sub m n

-- }}}

-- Mul Div {{{

type family Mul (m :: N) (n :: N) :: N where
  Mul    Z  n = Z
  Mul (S m) n = Add n (Mul m n)

type family Div (m :: N) (n :: N) :: N where
  Div    Z  (S n) = Z
  Div (S m) (S n) = Succ (Div (Sub (S m) (S n)) (S n))

-- }}}

-- Choose {{{

type family Choose (n :: N) (k :: N) :: N where
  Choose    n     Z  = S Z
  Choose    Z  (S k) = Z
  Choose (S n) (S k) = Add (Choose n (S k)) (Choose n k)

-- }}}

-- Functions {{{

[proxify|
  suc    ::      n ->       Succ n
  prd    ::      n ->       Pred n
  add    :: m -> n ->    m `Add` n
  sub    :: m -> n ->    m `Sub` n
  mul    :: m -> n ->    m `Mul` n
  div    :: m -> n ->    m `Div` n
  choose :: n -> k -> n `Choose` k
|]

-- }}}

data Eqv a b where
  Eqv :: (a ~ b) => Eqv a b

data Nat (n :: N) where
  Nz :: Nat Z
  Ns :: IsNat n => Nat n -> Nat (S n)

addZ :: Nat n -> Eqv (Add n Z) n
addZ n = case n of
  Nz -> Eqv
  Ns n' -> case addZ n' of Eqv -> Eqv

commAdd :: Nat m -> Nat n -> Eqv (Add m n) (Add n m)
commAdd m n = case (m,n) of
  (Nz,Nz)       -> Eqv
  (Nz,Ns n')    -> case addZ n' of Eqv -> Eqv
  (Ns m',Nz)    -> case addZ m' of Eqv -> Eqv
  (Ns m',Ns n') -> case commAdd m' n' of
    Eqv -> case (commAdd (Ns m') n',commAdd m' (Ns n')) of
      (Eqv,Eqv) -> Eqv

mulZ :: Nat n -> Eqv (Mul n Z) Z
mulZ n = case n of
  Nz -> Eqv
  Ns n' -> case mulZ n' of Eqv -> Eqv

commMul :: Nat m -> Nat n -> Eqv (Mul m n) (Mul n m)
commMul m n = case (m,n) of
  (Nz,Nz)       -> Eqv
  (Nz,Ns n')    -> case mulZ n' of Eqv -> Eqv
  (Ns m',Nz)    -> case mulZ m' of Eqv -> Eqv
  (Ns m',Ns n') -> case commMul m' n' of
    Eqv -> undefined

