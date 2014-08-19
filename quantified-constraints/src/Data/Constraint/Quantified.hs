{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Constraint.Quantified where

import Prelude hiding (replicate)
import Control.Applicative
import GHC.Exts (Constraint)
import Data.Type.Equality
import Data.Type.Bool
import Unsafe.Coerce

class Quant (c :: k -> Constraint) (f :: l -> k) where
  inst :: Dict (c (f a))

data Dict (c :: Constraint) where
  Dict :: c => Dict c

(\\) :: (c => r) -> Dict c -> r
r \\ Dict = r
infixl 1 \\

class Implies
  (c :: k -> Constraint)
  (d :: l -> Constraint)
  (f :: k -> l) where
  impl :: Dict (c a) -> Dict (d (f a))

-- Nat {{{

data N
  = Z
  | S N

data Nat (n :: N) where
  Z_ :: Nat Z
  S_ :: IsNat n => Nat n -> Nat (S n)

type family (x :: N) <=? (y :: N) :: Bool where
  Z   <=?   y = True
  S x <=? Z   = False
  S x <=? S y = x <=? y

class ((x <=? y) ~ True) => (x :: N) <= (y :: N) where
  leProof :: LeR x y

instance Z <= Z where
  leProof = LeZR

data B (b :: Bool) where
  True_  :: B True
  False_ :: B False

boolProp :: forall (x :: N) (y :: N). B (x == y) -> Maybe (x :~: y)
boolProp b = case b of
  True_  -> Just $ unsafeCoerce Refl
  False_ -> Nothing

propBool :: forall (x :: N) (y :: N). Maybe (x :~: y) -> B (x == y)
propBool p = case p of
  Just Refl -> unsafeCoerce True_
  _         -> unsafeCoerce False_

{-
instance
  ( (x  == y) ~ p
  ,  If p (x' ~ S x) (x' ~ x)
  ,  x' <= y
  ) => x' <= S y where
-}

{-
instance (x <= y, x ~ y) => S x <= S y where
  leProof = LeSSR leProof
-}

{-
instance (x <= y) => S x <= S y where
-}

data LeL (x :: N) (y :: N) where
  LeZL :: LeL Z y
  LeSL :: LeL x y -> LeL (S x) (S y)

data LeR (x :: N) (y :: N) where
  LeZR  :: LeR Z Z
  LeSSR :: LeR x x -> LeR (S x) (S x)
  LeSR  :: LeR x y -> LeR    x  (S y)

class IsNat (n :: N) where
  nat :: Nat n

instance IsNat Z where
  nat = Z_

instance IsNat n => IsNat (S n) where
  nat = S_ nat

data NatRec (c :: N -> Constraint) (x :: N) = NatRec
  { base :: Dict (c x)
  , ind  :: forall (n :: N). Dict (c n) -> Dict (c (S n))
  }

class QuantN (c :: N -> Constraint) (x :: N) where
  instN :: NatRec c x

class    c (f a) => ((c :: l -> Constraint) :.: (f :: k -> l)) (a :: k)
instance c (f a) => (c :.: f) a

-- }}}

-- Vec {{{

data Vec (n :: N) a where
  Nil  :: Vec Z a
  (:*) :: a -> Vec n a -> Vec (S n) a
infixr 4 :*

newtype V a (n :: N) = V { vec :: Vec n a }

instance TestEquality (V a) where
  testEquality (V u) (V v) = case (u,v) of
    (Nil,Nil)        -> Just Refl
    (_ :* u',_:* v') -> case testEquality (V u') (V v') of
      Just Refl -> Just Refl
      _         -> Nothing
    _                -> Nothing

instance Functor (Vec n) where
  fmap f v = case v of
    Nil -> Nil
    a :* v' -> f a :* fmap f v'

instance Quant Functor Vec where
  inst = Dict

replicate :: forall n a. IsNat n => a -> Vec n a
replicate a = go (nat :: Nat n)
  where
  go :: forall n. Nat n -> Vec n a
  go n = case n of
    Z_    -> Nil
    S_ n' -> a :* go n'

instance Applicative (Vec Z) where
  pure _ = Nil
  _ <*> _ = Nil

instance Applicative (Vec n) => Applicative (Vec (S n)) where
  pure a = a :* pure a
  (f :* vf) <*> (x :* vx) = f x :* (vf <*> vx)

instance QuantN (Applicative :.: Vec) Z where
  instN = NatRec
    { base = Dict
    , ind  = \Dict -> Dict
    }

{-
select :: forall c x y. (x <= y) => NatRec c x -> Dict (c y)
select = go (leProof :: Le x y)
  where
  go :: forall w z. Le w z -> NatRec c w -> Dict (c z)
  go le p@(NatRec b i) = case le of
    LeZL     -> _
    LeSL le' -> undefined
-}

{-
(\\#) :: forall c x y r. (y <= x) => (c x => r) -> NatRec c y -> r
r \\# p@(NatRec b i) = go (leProof :: Le y x)
  where
  go :: forall z. Le y z -> NatRec c z -> r
  go le = case le of
    LeZL     -> _
    LeSL le' -> _
-}

-- }}}

