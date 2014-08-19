{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Theorems.Nat where

import Type.Nat

import Theorems.Base

-- Add {{{

{-
addL :: Nat z -> x :== y -> x :+ z :== y :+ z
addL z p = case z of
  NZ    -> p |- refl
  NS z' -> addL z' p |- refl
-}

addZ :: Nat x -> x :+ Z :== x
addZ x = case x of
  NZ    -> refl
  NS x' -> addZ x' |- refl

addS :: Nat x -> Nat y -> S x :+ y :== x :+ S y
addS x y = case x of
  NZ    -> refl
  NS x' -> addS x' y |- refl

addComm :: Nat x -> Nat y -> x :+ y :== y :+ x
addComm x y = case (x,y) of
  (NZ   ,NZ   ) -> refl
  (NZ   ,NS y') -> addZ y' |- refl
  (NS x',NZ   ) -> addZ x' |- refl
  (NS x',NS y') ->
       addComm x' y'
     & addComm x' (NS y')
     & addComm (NS x') y'
     |- refl

addAssoc :: Nat x -> Nat y -> Nat z
  -> (x :+ y) :+ z :== x :+ (y :+ z)
addAssoc x y z = case x of
  NZ -> refl
  NS x' -> addZ x'
    |- case y of
      NZ    -> refl
      NS y' -> addZ y'
        |- case z of
          NZ    -> addZ (x' +: y) |- refl
          NS z' ->
               addAssoc x' y' z'
             & addS x' y
             & addAssoc x' y z
             |- refl

addPermute :: Nat x -> Nat y -> Nat z
  -> x :+ (y :+ z) :== y :+ (x :+ z)
addPermute x y z =
     addAssoc x y z
   & addComm x y
   & addAssoc y x z
   |- refl

eqS :: x :== y -> S x :== S y
eqS Refl = refl

addZZ :: x :== y -> x :+ Z :== y :+ Z
addZZ Refl = refl

addSimpR :: Nat x -> Nat y -> Nat z
  -> (x :+ y :== x :+ z)
  -> (y :== z)
addSimpR x y z Refl = case x of
  NZ -> refl
  NS x' -> addSimpR x' y z refl

addSimpL :: Nat x -> Nat y -> Nat z
  -> (y :+ x :== z :+ x)
  -> (y :== z)
addSimpL x y z p =
    addComm y x
  & addComm z x
  |- addSimpR x y z p

inAddR :: Nat x -> (y :== z)
  -> (x :+ y :== x :+ z)
inAddR x p = case x of
  NZ    -> p |- refl
  NS x' -> eqS $ inAddR x' p

inAddL :: Nat x
  -> (y :== z)
  -> (y :+ x :== z :+ x)
inAddL x p = case x of
  NZ    -> p |- refl
  NS x' -> _

-- }}}

-- Mul {{{

mulZ :: Nat x -> x :* Z :== Z
mulZ x = case x of
  NZ    -> refl
  NS x' -> mulZ x'

mulS :: Nat x -> Nat y -> (y :* x) :+ x :== S y :* x
mulS x y = case x of
  NZ -> case y of
    NZ    -> refl
    NS y' -> mulZ y' |- refl

mulSZ :: Nat x -> x :* S Z :== x
mulSZ x = case x of
  NZ    -> refl
  NS x' -> mulSZ x' |- refl

{-
mulComm :: Nat x -> Nat y -> x :* y :== y :* x
mulComm (x :: Nat x) (y :: Nat y) = case (x,y) of
  (NZ,_)        -> mulZ y |- refl
  (_,NZ)        -> mulZ x |- refl
  (NS (x' :: Nat x'), NS (y' :: Nat y')) ->
-}

{-
  given m,n > 1:
  y*x            : given
= y'*x + x       : mulS x y'
= x*y' + x       : mulComm y' x
= x'*y' + y' + x : mulS y' x'
= x'*y' + x + y' : addComm y' x
= x'*y' + x' + y : addS x' y'
= y'*x' + x' + y : mulComm x' y'
= y*x' + y       : mulS x' y'
= x'*y + y       : mulComm x' y
= x*y            : mulS y x'
-}

{-
mulDist :: Nat x -> Nat y -> Nat z
  -> x :* (y :+ z) :== (x :* y) :+ (x :* z)
mulDist x y z = case x of
  NZ -> refl
  NS x' -> case y of
    NZ -> mulZ x' |- refl
    NS y' -> case z of
      NZ -> inAddR (y' +: x' *: y) (x' *: NZ) NZ (mulZ x')
        |- inAddL (x' *: NS (y' +: z)) (y' +: NZ) NZ (addZ y')
        |- refl
-}


-- }}}

