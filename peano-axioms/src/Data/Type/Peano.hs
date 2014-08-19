{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Type.Peano where

import GHC.TypeLits hiding (type (<=))
import qualified GHC.TypeLits as TL

import Data.Type.Equality
import Data.Void
import Prelude hiding (succ,pred,(<=))
import Unsafe.Coerce
import Data.Proxy

type Not p   = p -> Void
type a :/: b = Not (a :~: b)

-- Peano Axioms {{{

data Peano (x :: Nat) where
  -- | Axiom 1
  Z :: Peano 0
  -- | Axiom 6
  S :: Peano x -> Peano (Succ x)

-- | Axiom 2
natRefl :: Peano x -> (x :~: x)
natRefl _ = Refl

-- | Axiom 3
natSymm :: Peano x -> Peano y -> (x :~: y) -> (y :~: x)
natSymm _ _ = sym

-- | Axiom 4
natTrans :: Peano x -> Peano y -> Peano z
  -> (x :~: y) -> (y :~: z) -> (x :~: z)
natTrans _ _ _ = trans

-- | Axiom 5
natEq :: Peano x -> (x :~: y) -> Peano y
natEq x Refl = x

-- | Axiom 7
succ0 :: Peano x -> Succ x :/: 0
succ0 _ Refl = error "succ0: S x /= 0"

_Z :: 0 :~: 0
_Z = Refl

_S :: x :~: y -> Succ x :~: Succ y
_S Refl = Refl

type Succ (x :: Nat) = 1 + x

succ :: KnownNat x => Peano x -> Peano (Succ x)
succ = S

{-
pred :: (KnownNat x, 1 <= x) => Peano x -> Peano (Pred x)
pred x = case x of
  S x' -> _
-}

{-
-- | Axiom 8
_Succ :: Peano x -> Peano y -> (Succ x :~: Succ y) -> x :~: y
_Succ x y Refl = case x of
  Z    -> _
  S x' -> _
-}

{-
addSub :: Peano x -> Peano y -> ((x + y) - y) :~: x
addSub x y = 
-}

type Pred (x :: Nat) = x - 1

-- | Axiom 9
natElim :: p 0 -> (forall x. Peano x -> p x -> p (Succ x))
  -> Peano x -> p x
natElim z s x = case x of
  Z    -> z
  S x' -> s x' $ natElim z s x'

{-
natElimLe :: (x <= y) => p x -> (forall x. Peano x -> p x -> p (Succ x))
  -> Peano y -> p y
natElimLe z s y = case y of
  Z    -> _
  S y' -> _
-}

predSucc :: Peano x -> Pred (Succ x) :~: x
predSucc x = case x of
  Z    -> _Z
  S x' -> case predSucc x' of
    Refl -> _

succPred :: (1 <= x) => Peano x -> Succ (Pred x) :~: x

-- }}}

-- Addition Axioms {{{

add0 :: Peano x -> (x + 0) :~: x
add0 _ = Refl

addSucc :: Peano x -> Peano y -> (x + Succ y) :~: Succ (x + y)
addSucc _ _ = unsafeCoerce Refl

newtype AddSub (x :: Nat) = AddSub
  { unAddSub :: Pred (Succ x) :~: x
  }

{-
addSub :: forall x y. Peano x -> Peano y -> ((x + y) - y) :~: x
addSub x y = case y of
  Z    -> natRefl x
  S y' -> _
-}

-- }}}

-- <= {{{

data (x :: Nat) :<=: (y :: Nat) where
  LeZ ::  Peano y ->      0 :<=:      y
  LeS :: x :<=: y -> Succ x :<=: Succ y

leRefl :: Peano x -> x :<=: x
leRefl x = case x of
  Z    -> LeZ x
  S x' -> LeS $ leRefl x'

leAntiSymm :: forall x y. x :<=: y -> y :<=: x -> x :~: y
leAntiSymm xy yx = case (leLeft xy, leRight xy) of
  (Z   ,Z   ) -> _Z
  (Z   ,S y') -> _
  (S x',Z   ) -> _
  (S x',S y') -> _

leLeft :: x :<=: y -> Peano x
leLeft le = case le of
  LeZ _   -> Z
  LeS le' -> S $ leLeft le'

leRight :: x :<=: y -> Peano y
leRight le = case le of
  LeZ y   -> y
  LeS le' -> S $ leRight le'

{-
(<=) :: Peano x -> Peano y -> Guard (x <=? y) (x :<=: y)
x <= y = case (x,y) of
  (Z   ,_   ) -> KnownTrue $ LeZ y
  (S x',Z   ) -> undefined
  (S x',S y') -> case x' <= y' of
    KnownTrue  p -> case p of
      LeZ dy -> _
      LeS le -> _
    KnownFalse   -> undefined
    Unknown    p -> undefined
-}

{-
case y of
  S y' -> _ -- fmap LeS $ x <= y'
  Z    -> case x of
    Z    -> KnownTrue $ LeN x
    S x' -> _ -- Nothing
-}

-- }}}

data Guard (p :: Bool) (a :: *) where
  KnownTrue  ::              a  -> Guard True  a
  KnownFalse ::                    Guard False a
  Unknown    :: (p ~ True => a) -> Guard p a

instance Functor (Guard p) where
  fmap f gd = case gd of
    KnownTrue  a -> KnownTrue  $ f a
    KnownFalse   -> KnownFalse 
    Unknown    a -> Unknown    $ f a

