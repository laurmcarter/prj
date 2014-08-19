{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Data.Type.Natural where

import Data.Type.Number.Class
import Data.Type.Equality

import Control.Lens hiding (over)
import qualified Control.Lens as Lens

data N
  = Z
  | S N
  deriving (Eq,Show)

data Nat (x :: N) where
  Z_ :: Nat Z
  S_ :: Nat x -> Nat (S x)

data (x :: N) <= (y :: N) where
  LeZ :: Nat x -> Z <= x
  LeS :: x <= y -> S x <= S y

type instance Z   + y = y
type instance S x + y = S (x + y)

instance NumAdd Nat where
  type N0 Nat = Z
  x +: y = case x of
    Z_    -> y
    S_ x' -> S_ $ x' +: y
  addIdL _ = Refl
  addIdR x = case x of
    Z_    -> Refl
    S_ x' -> cong $ addIdR x'
  addAssoc x y z = case x of
    Z_    -> Refl
    S_ x' -> cong $ addAssoc x' y z
  addComm x y = case x of
    Z_    -> sym $ addIdR y
    S_ x' -> cong (addComm x' y) `trans` addS y x'

addS :: Nat x -> Nat y -> S (x + y) :~: x + S y
addS x y = case x of
  Z_    -> Refl
  S_ x' -> cong $ addS x' y

induct ::
     p Z
  -> (forall (n :: N). Nat n -> p n -> p (S n))
  -> Nat x 
  -> p x
induct z s x = case x of
   Z_    -> z
   S_ x' -> s x' $ induct z s x'

newtype AddZ c x = AddZ
  { addZ :: c (x + Z) x
  }

nat :: x :~: y -> Nat x :~: Nat y
nat Refl = Refl

unNat :: Nat x :~: Nat y -> x :~: y
unNat Refl = Refl

foo :: forall x. Nat x -> x + Z :~: x
foo = addZ
  . induct
    (AddZ Refl)
    (const $ AddZ . cong . addZ)

viewTo :: AnIso s t a b -> s -> a
viewTo = view . coerced . cloneIso

viewFrom :: AnIso s t a b -> b -> t
viewFrom = view . coerced . from . cloneIso

over :: AnIso s t a b -> (a -> b) -> s -> t
over = Lens.over . cloneIso

-- under :: AnIso s t a b -> (t -> s) -> (b -> a)

type instance Z   * y = Z
type instance S x * y = y + (x * y)

instance NumMul Nat where
  type N1 Nat = S Z
  x *: y = case x of
    Z_    -> Z_
    S_ x' -> y +: x' *: y
  mulIdL x = case x of
    Z_    -> Refl
    S_ x' -> cong $ mulIdL x'
  mulIdR x = case x of
    Z_    -> Refl
    S_ x' -> cong $ mulIdR x'
  distMul x y z = case x of
    Z_    -> Refl
    S_ x' -> undefined

