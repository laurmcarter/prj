{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module Data.Functor.Num where

import Data.Functor.Classes.Extra
import Prelude hiding ((+),(-),(*))
import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeLits (Nat)
import qualified GHC.TypeLits as Lit
import Data.Proxy

type family (x :: k) + (y :: k) :: k
type family (x :: k) - (y :: k) :: k
type family (x :: k) * (y :: k) :: k
type family Negate     (x :: k) :: k
type family Abs        (x :: k) :: k
type family Signum     (x :: k) :: k
type family Zero                :: k
type family One                 :: k
type family FromInteger         :: k -> Constraint

infixl 6 +
infixl 6 -
infixl 7 *

class NumF (t :: k -> *) where
  (+)          :: t a -> t b -> t (a + b)
  (-)          :: t a -> t b -> t (a - b)
  (*)          :: t a -> t b -> t (a * b)
  negateT      :: t a -> t (Negate a)
  signumT      :: t a -> t (Signum a)
  absT         :: t a -> t (Abs    a)
  fromIntegerT :: Integer -> Maybe (Where FromInteger t)

class VerifiedNumF (t :: k -> *) where
  dblNeg     :: t a -> Negate (Negate a) :~: a
  addSub     :: t a -> t b -> (a - b) :~: (a + Negate b)
  add0Left   :: t a -> (Zero + a)  :~: a
  add0Right  :: t a -> (a + Zero)  :~: a
  mul1Left   :: t a -> (One  * a)  :~: a
  mul1Right  :: t a -> (a *  One)  :~: a
  absIdem    :: t a -> Abs (Abs a) :~: Abs a
  signumVals :: t a -> Poss ((:~:) (Signum a)) '[Zero,One,Negate One]

type instance x + y = x Lit.+ y
type instance x * y = x Lit.* y
type instance x - y = x Lit.- y
type instance Negate (x :: Nat) = 0 - x
type instance Abs    (x :: Nat) = If (0 Lit.<=? x) x (Negate x)
type instance Signum (x :: Nat) = If (x == 0) 0 (If (x Lit.<=? 0) (Negate 1) 1)
type instance Zero              = 0
type instance One               = 1
type instance FromInteger       = Lit.KnownNat

instance NumF (Proxy :: Nat -> *) where
  _ + _ = Proxy
  _ - _ = Proxy
  _ * _ = Proxy
  negateT _ = Proxy
  signumT _ = Proxy
  absT    _ = Proxy
  fromIntegerT i = case Lit.someNatVal i of
    Just (Lit.SomeNat p) -> Just $ Where p
    _                    -> Nothing

