{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Type.Nat where

import Control.Category
import Prelude hiding
  ( divMod,compare,succ,pred,id,(.),length
  , (<=),(>=),mod,gcd
  )
import Data.Type.Bool
import Data.Type.Equality
import Data.Proxy

import Data.Constraint.Extra
import Data.Constraint.Proof
import Data.Constraint.Sum

-- N Nat (:~:) {{{

data N
  = Z
  | S N
  deriving (Eq,Ord,Show)

data Nat (n :: N) where
  Z_ :: Nat Z
  S_ :: Nat n -> Nat (S n)

deriving instance Show (Nat n)

eqZ :: Z :~: Z
eqZ = Refl

eqS :: x :~: y -> S x :~: S y
eqS Refl = Refl

eqP :: S x :~: S y -> x :~: y
eqP Refl = Refl

-- }}}

-- (:/:) {{{

neS :: x :/: y -> S x :/: S y
neS = (NeSS \\)

neP :: forall x y. S x :/: S y -> x :/: y
neP NeSS = neProof (Proxy :: Proxy x) (Proxy :: Proxy y)

instance Proof (Nat x) where
  type ProofOf (Nat x) = KnownNat x
  type Residue (Nat x) = EmptyC
  x // r = case x of
    Z_    -> r
    S_ x' -> x' // r

type instance x == y = NatEq x y
type x /=? y         = Not (x == y)

type family NatEq (x :: N) (y :: N) :: Bool where
  NatEq  Z     Z    = True
  NatEq  Z    (S y) = False
  NatEq (S x)  Z    = False
  NatEq (S x) (S y) = NatEq x y

type (:/:) = Ne

data Ne (x :: N) (y :: N) where
  NeZS :: Nat (S y) -> Ne  Z    (S y)
  NeSZ :: Nat (S x) -> Ne (S x)  Z
  NeSS :: (x /= y)  => Ne (S x) (S y)

instance Proof (Ne x y) where
  type ProofOf (Ne x y) = x /= y
  type Residue (Ne x y) = EmptyC
  ne // r = case ne of
    NeZS y -> r \\ y
    NeSZ x -> r \\ x
    NeSS   -> r

newtype NeKnown x y = NeKnown
  { knownNe :: Ne x y
  }

instance Proof (NeKnown x y) where
  type ProofOf (NeKnown x y) = (KnownNat x, KnownNat y)
  type Residue (NeKnown x y) = EmptyC
  NeKnown ne // r = case ne of
    NeZS y -> r \\ y
    NeSZ x -> r \\ x
    NeSS   -> r \\ NeKnown (neP ne)
    

class (Prop (x /=? y), NeC x y) => (x :: N) /= (y :: N) where
  type NeC x y :: Constraint
  neProof :: p0 x -> p1 y -> Ne x y

instance KnownNat y => Z /= S y where
  type NeC Z (S y) = KnownNat y
  neProof _ _ = NeZS nat

instance KnownNat x => S x /= Z where
  type NeC (S x) Z = KnownNat x
  neProof _ _ = NeSZ nat

instance (x /= y) => S x /= S y where
  type NeC (S x) (S y) = (x /= y)
  neProof _ _ = NeSS

-- }}}

-- Known {{{

class NatC n => KnownNat (n :: N) where
  type NatC n :: Constraint
  nat :: Nat n

instance KnownNat Z where
  type NatC Z = EmptyC
  nat = Z_

instance KnownNat n => KnownNat (S n) where
  type NatC (S n) = KnownNat n
  nat = S_ nat

-- }}}

-- (+) (*) {{{

type family (x :: N) + (y :: N) :: N where
  x + Z   = x
  x + S y = S (x + y)
infixl 6 +

type family (x :: N) * (y :: N) :: N where
  x * Z   = Z
  x * S y = (x * y) + x
infixl 7 *

(+:) :: Nat x -> Nat y -> Nat (x + y)
x +: y  = case y of
  Z_    -> x
  S_ y' -> S_ $ x +: y'
infixl 6 +:

(*:) :: Nat x -> Nat y -> Nat (x * y)
x *: y  = case y of
  Z_    -> Z_
  S_ y' -> x *: y' +: x
infixl 7 *:

-- }}}

-- (-) {{{

type family (x :: N) - (y :: N) :: N where
  x   - Z   = x
  S x - S y = x - y

(-:) :: (y <= x) => Nat x -> Nat y -> Nat (x - y)
x -: y = case leProof y x of
  LeZ -> x
  LeS -> case (x,y) of
    (S_ x',S_ y') -> x' -: y'
    _             -> unreachable
    -- Absurd:
    -- (Z_  ,_ ) : S x /~ Z
    -- (S_ _,Z_) : S x /~ Z

-- }}}

-- (/) mod {{{

type family DivMod (x :: N) (y :: N) :: (N,N) where
  DivMod Z y = '(Z,Z)
  DivMod x y = If (x <=? y) '(Z,x) (SFst (DivMod (x - y) y))

type family SFst (p :: (N,N)) :: (N,N) where
  SFst '(x,y) = '(S x,y)

divMod :: forall x y. Nat x -> Nat y -> Pair' Nat (DivMod x y)
divMod x y = case x of
  Z_    -> Pair Z_ Z_
  S_ x' -> case x <= y of
    Yes xy -> Pair Z_ x \\ xy
    No     -> case y <= x of
      Yes yx -> case divMod x' y of
        Pair d r -> Pair (S_ d) r
        where
        x' :: Nat (x - y)
        x' = x -: y \\ yx
      _ -> unreachable
      -- Absurd:
      -- No : x <= y || y <= x must hold

type Div x y = Fst (DivMod x y)
type Mod x y = Snd (DivMod x y)

(/:) :: forall x y d r. ('(d,r) ~ DivMod x y)
  => Nat x -> Nat y -> Nat d
x /: y = fst' $ divMod x y
infixl 7 /:

mod :: forall x y d r. ('(d,r) ~ DivMod x y)
  => Nat x -> Nat y -> Nat r
mod x y = snd' $ divMod x y

-- }}}

-- Fact {{{

type family Fact (x :: N) :: N where
  Fact Z     = S Z
  Fact (S x) = Fact x * S x

fact :: Nat x -> Nat (Fact x)
fact x = case x of
  Z_    -> S_ Z_
  S_ x' -> fact x' *: x

-- }}}

-- GCD {{{

type family GCD (x :: N) (y :: N) :: N where
  GCD x y = If (x >? y) (GCD' y x) (GCD' x y)

type family GCD' (x :: N) (y :: N) :: N where
  GCD'  Z    y = y
  GCD' (S x) y = GCD' y (Mod (S x) y)

{-
gcd :: Nat x -> Nat y -> Nat (GCD x y)
gcd x y = case compare x y of
  Left  (xy,yx)    -> gcd' x y \\ xy
  Right (Left  xy) -> gcd' x y \\ xy
  Right (Right yx) -> gcd' y x \\ yx

gcd' :: (x < y) => Nat x -> Nat y -> Nat (GCD' x y)
gcd' x y = undefined -- case x of
  -- Z_    -> y
  -- S_ x' -> case divMod x y of
  --   Pair _ r -> gcd' y r
-}

-- }}}

-- Boolean Inequality {{{

invLeGt :: Nat x -> Nat y -> (x <=? y) :~: Not (x >? y)
invLeGt x y = case (x,y) of
  (Z_   ,Z_   ) -> Refl
  (Z_   ,S_ y') -> Refl
  (S_ x',Z_   ) -> Refl
  (S_ x',S_ y') -> Refl \\ invLeGt x' y'

invLtGe :: Nat x -> Nat y -> (x <? y) :~: Not (x >=? y)
invLtGe x y = case (x,y) of
  (Z_   ,Z_   ) -> Refl
  (Z_   ,S_ y') -> Refl
  (S_ x',Z_   ) -> Refl
  (S_ x',S_ y') -> Refl \\ invLtGe x' y'

invGeLt :: Nat x -> Nat y -> (x >=? y) :~: Not (x <? y)
invGeLt x y = case (x,y) of
  (Z_   ,Z_   ) -> Refl
  (Z_   ,S_ y') -> Refl
  (S_ x',Z_   ) -> Refl
  (S_ x',S_ y') -> Refl \\ invGeLt x' y'

invGtLe :: Nat x -> Nat y -> (x >? y) :~: Not (x <=? y)
invGtLe x y = case (x,y) of
  (Z_   ,Z_   ) -> Refl
  (Z_   ,S_ y') -> Refl
  (S_ x',Z_   ) -> Refl
  (S_ x',S_ y') -> Refl \\ invGtLe x' y'

type family (x :: N) <=? (y :: N) :: Bool where
  Z   <=?   y = True
  S x <=? Z   = False
  S x <=? S y = x <=? y

type x >=? y =   y <=? x
type x <?  y = S x <=? y
type x >?  y =   y <?  x

(<=:) :: Nat x -> Nat y -> B (x <=? y)
x <=: y = case (x,y) of
  (Z_   ,_    ) -> True_
  (S_ x',Z_   ) -> False_
  (S_ x',S_ y') -> x' <=: y'

(>=:) :: Nat x -> Nat y -> B (x >=? y)
x >=: y = case (x,y) of
  (Z_   ,Z_   ) -> True_
  (Z_   ,S_ y') -> False_
  (S_ x',Z_   ) -> True_
  (S_ x',S_ y') -> x' >=: y'

(<:) :: Nat x -> Nat y -> B (x <? y)
x <: y = case (x,y) of
  (Z_   ,Z_   ) -> False_
  (Z_   ,S_ y') -> True_
  (S_ x',Z_   ) -> False_
  (S_ x',S_ y') -> x' <: y'

(>:) :: Nat x -> Nat y -> B (x >? y)
x >: y = case (x,y) of
  (Z_   ,Z_   ) -> False_
  (Z_   ,S_ y') -> False_
  (S_ x',Z_   ) -> True_
  (S_ x',S_ y') -> x' >: y'

-- }}}

-- Propositional Inequality {{{

type Lt x y = Le (S x)   y
type Ge x y = Le    y    x
type Gt x y = Ge    x (S y)

data Le (x :: N) (y :: N) where
  LeZ :: KnownNat y => Le  Z       y
  LeS :: (x <= y)   => Le (S x) (S y)


instance Proof (Le x y) where
  type ProofOf (Le x y) = (x <= y, Prop (x <=? y))
  type Residue (Le x y) = EmptyC
  le // r = case le of
    LeZ -> r
    LeS -> r

-- Wrapper to denote that we want to recover Nat
-- values from Le term
newtype LeKnown x y = LeKnown
  { knownLe :: Le x y
  }

instance Proof (LeKnown x y) where
  type ProofOf (LeKnown x y) = (KnownNat x, KnownNat y, ProofOf (Le x y))
  type Residue (LeKnown x y) = EmptyC
  LeKnown le // r = case le of
    LeZ -> r
    LeS -> LeKnown (leP le) // r


leRefl :: forall x. KnownNat x => Le x x
leRefl = case (nat :: Nat x) of
  Z_                -> LeZ
  S_ (x' :: Nat x') -> LeS \\ (leRefl :: Le x' x') \\ x'

leTrans :: Le y z -> Le x y -> Le x z
leTrans yz xy = case xy of
  LeZ -> LeZ \\ LeKnown yz
  LeS -> case yz of
    LeS -> LeS \\ leTrans (leP yz) (leP xy)
    _ -> unreachable
    -- Absurd:
    -- LeZ      : S x /~ Z


leS :: forall x y. Le x y -> Le (S x) (S y)
leS = (LeS \\)

leP :: forall x y. Le (S x) (S y) -> Le x y
leP LeS = leProof (Proxy :: Proxy x) (Proxy :: Proxy y)


class (Prop (x <=? y), Prop (Not (x >? y)), LeC x y)
  => (x :: N) <= (y :: N) where
  type LeC x y :: Constraint
  leProof :: p0 x -> p1 y -> Le x y

instance KnownNat y => Z <= y where
  type LeC Z y = KnownNat y
  leProof _ _ = LeZ

instance (x <= y) => S x <= S y where
  type LeC (S x) (S y) = x <= y
  leProof _ _ = LeS


class (y <= x) => (x :: N) >= (y :: N) where
  geProof :: p0 x -> p1 y -> Ge x y
instance (y <= x) => x >= y where
  geProof = flip leProof

class (S x <= y) => (x :: N) < (y :: N) where
  ltProof :: p0 x -> p1 y -> Lt x y
instance (S x <= y) => x < y where
  ltProof _ = leProof (Proxy :: Proxy (S x))

class (x >= S y) => (x :: N) > (y :: N) where
  gtProof :: p0 x -> p1 y -> Gt x y
instance (x >= S y) => x > y where
  gtProof x _ = geProof x (Proxy :: Proxy (S y))



-- The result of compare is the power set of { Le x y, Le y x }, with at
-- least one element.
-- This is isomorphic to the fact that two natural numbers will either be
-- inequal, where one is (<=) to the other,
-- or equal, where both are (<=) to the other.
compare :: Nat x -> Nat y -> Either (Le x y,Le y x) (Either (Le x y) (Le y x))
compare x y = case (x <= y,y <= x) of
  (Yes xy,Yes yx) -> Left  (xy,yx)
  (Yes xy,No    ) -> Right $ Left xy
  (No    ,Yes yx) -> Right $ Right yx
  _ -> unreachable
    -- Absurd:
    -- (No,No)

(<=) :: Nat x -> Nat y -> Option (x <=? y) (Le x y)
x <= y = case x of
  Z_    -> Yes LeZ \\ y
  S_ x' -> case y of
    Z_    -> No
    S_ y' -> fmap leS $ x' <= y'
infix 4 <=

(>=) :: Nat x -> Nat y -> Option (y <=? x) (Le y x)
(>=) = flip (<=)
infix 4 >=

antiSymm :: Le x y -> Le y x -> x :~: y
antiSymm xy yx = case (xy,yx) of
  (LeZ,LeZ) -> eqZ
  (LeS,LeS) -> eqS $ antiSymm (leP xy) (leP yx)
  _         -> unreachable
  -- Absurd:
  -- (LeZ,LeS) : S x /~ Z
  -- (LeS,LeZ) : S x /~ Z

antiSymm' :: forall x y. (KnownNat x , KnownNat y)
  => x :~: y -> (Le x y, Le y x)
antiSymm' eq@Refl = case ((nat :: Nat x),(nat :: Nat y)) of
  (Z_,Z_)       -> (LeZ,LeZ)
  (S_ x',S_ y') -> (LeS,LeS) \\ antiSymm' (eqP eq) \\ (x',y')
  _ -> unreachable
  -- Absurd:
  -- (Z_  ,S_ _) : S x /~ Z
  -- (S_ _,Z_  ) : S x /~ Z

-- }}}

-- Pair {{{

data Pair (f :: k -> *) (g :: l -> *) :: (k,l) -> * where
  Pair :: f a -> g b -> Pair f g '(a,b)

type family Fst (p :: (k,l)) :: k where
  Fst '(a,b) = a

type family Snd (p :: (k,l)) :: l where
  Snd '(a,b) = b

first :: (f a -> h b) -> Pair f g '(a,c) -> Pair h g '(b,c)
first f (Pair a b) = Pair (f a) b

second :: (g a -> h b) -> Pair f g '(c,a) -> Pair f h '(c,b)
second f (Pair a b) = Pair a (f b)

fst' :: Pair f g p -> f (Fst p)
fst' (Pair a _) = a

snd' :: Pair f g p -> g (Snd p)
snd' (Pair _ b) = b

instance (Proof (f a), Proof (g b))
  => Proof (Pair f g '(a,b)) where
  type ProofOf (Pair f g '(a,b)) = (ProofOf (f a), ProofOf (g b))
  type Residue (Pair f g '(a,b)) = (Residue (f a), Residue (g b))
  Pair a b // r = a // b // r

type Pair' f = Pair f f

-- }}}

-- nat syns {{{

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

zero :: Nat N0
zero = Z_

one :: Nat N1
one = S_ zero

two :: Nat N2
two = S_ one

three :: Nat N3
three = S_ two

four :: Nat N4
four = S_ three

five :: Nat N5
five = S_ four

six :: Nat N6
six = S_ five

seven :: Nat N7
seven = S_ six

eight :: Nat N8
eight = S_ seven

nine :: Nat N9
nine = S_ eight

-- }}}

dblNeg :: B x -> x :~: Not (Not x)
dblNeg x = case x of
  True_  -> Refl
  False_ -> Refl

