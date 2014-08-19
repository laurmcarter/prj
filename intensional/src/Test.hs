{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RebindableSyntax #-}

module Test where

import Prelude.Indexed
import Type.Infer

import Type.Context
import Type.Equality
import Type.List
import Type.Nat
import Theorems.Nat (addZ)

type Theorem qs = Infer '[] qs ()
type Hypothesis as cs = Infer as (cs :++ as) ()

{-
addComm_ :: Nat x -> Nat y -> Theorem '[x :+ y :=: y :+ x]
addComm_ x y = case (x,y) of
  (NZ   ,NZ   ) -> triv
  (NZ   ,NS y') -> ()
-}

{-
  (NZ   ,NS y') -> addZ y' |- refl
  (NS x',NZ   ) -> addZ x' |- refl
  (NS x',NS y') ->
       addComm_ x' y'
     & addComm_ x' (NS y')
     & addComm_ (NS x') y'
     |- refl
-}

c0 :: Context '[]
c0 = NilC

c1 :: Nat x -> Context '[x :+ Z :=: x]
c1 x = addZ x :& NilC

type Goal1 = '[S (S (S Z)) :+ Z :=: S (S (S Z))]

c2 :: Context '[S Z :=: S Z]
c2 = refl :& NilC

c3 :: Context '[Z :=: Z, S Z :=: S Z]
c3 = refl :& refl :& NilC

c4 :: Context '[Z :=: Z, S Z :=: S Z, S (S Z) :=: S (S Z)]
c4 = refl :& refl :& refl :& NilC

triv :: Hypothesis ps '[a :=: a]
triv = Infer res
  where
  res :: Context ps -> (Presuming ps => ((),Context (a :=: a ': ps)))
  res ps = ((),(refl :: a :== a) & ps)

use :: forall ps a b. (Presuming ps => a :== b) -> Hypothesis ps '[a :=: b]
use eq = Infer res
  where
  res :: Context ps -> (Presuming ps => ((),Context (a :=: b ': ps)))
  res ps = ((),eq & ps)

given :: Context ps -> Infer ps qs () -> Context qs
given = flip proof

type Proof ps qs = forall rs. rs

mulDistZ :: Nat x -> Nat y -> Nat z
  -> Infer '[x :=: Z] '[x :* (y :+ z) :=: x :* y :+ x :* z] ()
mulDistZ = undefined

