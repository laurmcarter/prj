{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Type.Disequality where

import Type.Equality
import Type.Nat

import Data.Void

type x :/= y = x :== y -> Void

data (x :: N) :<= (y :: N) where
  LeRefl :: Nat x -> x :<= x
  LeS    :: Nat x -> Nat y -> x :<= y -> x :<= S y

leRefl :: Nat x -> x :<= x
leRefl = LeRefl

leS :: Nat x -> Nat y -> x :<= y -> x :<= S y
leS = LeS

type x :<  y = S x :<= y
type x :>= y =   y :<= x
type x :>  y =   y :<  x

