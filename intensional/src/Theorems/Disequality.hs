{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Theorems.Disequality where

import Type.Disequality
import Type.Nat

import Theorems.Base
import Theorems.Nat (addZ,addS)

{-
leBase :: x :<= y -> x :<= x
leBase p = case p of
  LeN    -> leRefl
  LeS p' -> leBase p'
-}

leZ :: Z :<= Z
leZ = leRefl NZ

{-
leAdd :: Nat x -> Nat y -> Nat z ->
  x :+ y :== z -> x :<= z
leAdd x y z Refl = case y of
  NZ    -> addZ x |- leRefl
  NS y' -> case (x,z) of
    (NZ,NS z') -> leRefl
-}

