{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Constraint.TypeLC where

import Data.Proxy
import GHC.Prim (Any)

data N
  = Z
  | S N
  deriving (Eq,Show)

data F (x :: k)

class A l a b | l a -> b

data FNot
instance A (F FNot) True  False
instance A (F FNot) False True

data FAnd
instance A (F FAnd) a (F '(FAnd,a))
instance A (F '(FAnd,True )) a a
instance A (F '(FAnd,False)) a False

data FOr
instance A (F FOr) a (F '(FOr,a))
instance A (F '(FOr,True )) a True
instance A (F '(FOr,False)) a a

data (f :: *) :< x
infixl 1 :<

class E a b | a -> b

instance E Z Z
instance E (x :: Bool)    x
instance E (F x)       (F x)

instance (E y y', A (F x) y' r)         => E ((F x) :< y) r


instance (E (x :< y) s, E (s :< z) r) => E ((x :< y) :< z) r

instance E (x :: N) (x' :: N) => E (S x) (S x')

data Rec l

instance E (l :< (F (Rec l)) :< x) r => A (F (Rec l)) x r

data FSum'

instance A (F FSum') self (F '(FSum',self))
instance A (F '(FSum',self)) n (F '(FSum',self,n))
instance A (F '(FSum',self,Z)) m m
instance E (self :< n :< m) r => A (F '(FSum',self,S n)) m (S r)

type FSum = Rec (F FSum')

testSum :: E (F FSum :< x :< y) r => Proxy x -> Proxy y -> Proxy r
testSum Proxy Proxy = Proxy

data Fib'

instance A (F Fib') self (F '(Fib',self))
instance A (F '(Fib',self))    Z  (S Z)
instance A (F '(Fib',self)) (S Z) (S Z)
instance E (F FSum :< (self :< n) :< (self :< (S n))) r
  => A (F '(Fib',self)) (S (S n)) r

type Fib = Rec (F Fib')

testFib :: E (F Fib :< x) r => Proxy x -> Proxy r
testFib Proxy = Proxy

type N0 = Z
type N1 = S N0
type N2 = S N1
type N3 = S N2
type N4 = S N3
type N5 = S N4
type N6 = S N5
type N7 = S N6
type N8 = S N7
type N9 = S N8

