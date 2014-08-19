{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Data.Type.Nat where

import Prelude hiding ((+),(*))
import Data.Type.Known
import Data.Type.Bool.Witness
import Data.Type.Equality
import Control.Lens
import Data.Profunctor

data N
  = Z
  | S N
  deriving (Eq,Ord,Show,Read)

data Nat (x :: N) where
  Z_ :: Nat Z
  S_ :: Nat x -> Nat (S x)

deriving instance Eq (Nat x)
deriving instance Ord (Nat x)
deriving instance Show (Nat x)

type instance x == y = NatEq x y
type family NatEq (x :: N) (y :: N) :: Bool where
  NatEq  Z     Z    = True
  NatEq  Z    (S y) = False
  NatEq (S x)  Z    = False
  NatEq (S x) (S y) = NatEq x y

instance Decidable Nat where
  decide a b = case (a,b) of
    (Z_   ,Z_   ) -> True_
    (Z_   ,S_ _ ) -> False_
    (S_ _ ,Z_   ) -> False_
    (S_ a',S_ b') -> decide a' b'

instance               Decide Z      Z    where _Decide = decideTrue
instance               Decide Z     (S b) where _Decide = decideFalse
instance               Decide (S a)  Z    where _Decide = decideFalse
instance Decide a b => Decide (S a) (S b) where
  _Decide = (_Decide :: Iso' (B (a == b)) (Maybe (a :~: b))) . iso ( ==> Just Refl ) ( ==> Just Refl )

-- Known {{{

instance Known Nat Z where
  known    = Z_

instance Known Nat n => Known Nat (S n) where
  known    = S_ known

-- }}}

-- Add {{{

type family (x :: N) + (y :: N) :: N where
  Z   + y = y
  S x + y = S (x + y)

(+) :: Nat x -> Nat y -> Nat (x + y)
x + y = case x of
  Z_    -> y
  S_ x' -> S_ $ x' + y

-- }}}

-- Mult {{{

type family (x :: N) * (y :: N) :: N where
  Z   * y = Z
  S x * y = y + (x * y)

(*) :: Nat x -> Nat y -> Nat (x * y)
x * y = case x of
  Z_    -> Z_
  S_ x' -> y + (x' * y)

-- }}}

-- Factorial {{{

type family Factorial (x :: N) :: N where
  Factorial Z     = S Z
  Factorial (S x) = S x * Factorial x

fact :: Nat x -> Nat (Factorial x)
fact x = case x of
  Z_    -> S_ Z_
  S_ x' -> x * fact x'

-- }}}

-- Le Lt {{{

data Le (x :: N) (y :: N) where
  LeZ :: Nat y  -> Le Z y
  LeS :: Le x y -> Le (S x) (S y)

class (x :: N) <= (y :: N) where
  leEvidence :: Le x y

instance Known Nat y => (Z <= y) where
  leEvidence = LeZ known

instance (x <= y) => (S x <= S y) where
  leEvidence = LeS leEvidence

type x < y  = S x <= y

pattern Lt e = LeS e

ltEvidence :: (x < y) => Le (S x) y
ltEvidence = leEvidence

type x >  y = y < x
type x >= y = y <= x

-- }}}

(==>) :: Maybe (a :~: b) -> (a ~ b => Maybe r) -> Maybe r
mp ==> r = case mp of
  Just Refl -> r
  _         -> Nothing

