
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Type.Number.Class where

import Control.Lens
import Data.Type.Equality
import GHC.Exts (Any)

type family (x :: k) + (y :: k) :: k
infixl 6 +

type family (x :: k) * (y :: k) :: k
infixl 7 *

type family (x :: k) - (y :: k) :: k
infixl 6 -
type family Neg (x :: k) :: k

type family (x :: k) / (y :: k) :: k
infixl 7 /
type family Recip (x :: k) :: k

-- TF Lenses {{{

newtype Id a = Id
  { unId :: a
  } deriving (Eq,Show)
_Id :: Iso a b (Id a) (Id b)
_Id = iso Id unId

newtype L (c :: k -> *) (a :: l) (f :: l -> k) = L
  { unL :: c (f a)
  } deriving (Eq,Show)
_L :: Iso (p (f a)) (q (g b)) (L p a f) (L q b g)
_L = iso L unL

newtype R (c :: k -> *) (f :: l -> k) (a :: l) = R
  { unR :: c (f a)
  } deriving (Eq,Show)
_R :: Iso (p (f a)) (q (g b)) (R p f a) (R q g b)
_R = iso R unR

newtype Add (c :: k -> *) (x :: k) (y :: k) = Add
  { unAdd :: c (x + y)
  } deriving (Eq,Show)
_Add :: Iso (p (x1 + y1)) (q (x2 + y2)) (Add p x1 y1) (Add q x2 y2)
_Add = iso Add unAdd

newtype Zero (c :: k -> *) (n :: k -> *) = Zero
  { unZero :: c (N0 n)
  } deriving (Eq,Show)

newtype Mul (c :: k -> *) (x :: k) (y :: k) = Mul
  { unMul :: c (x * y)
  } deriving (Eq,Show)
_Mul :: Iso (p (x1 * y1)) (q (x2 * y2)) (Mul p x1 y1) (Mul q x2 y2)
_Mul = iso Mul unMul

newtype One (c :: k -> *) (n :: k -> *) = One
  { unOne :: c (N1 n)
  } deriving (Eq,Show)

newtype Sub (c :: k -> *) (x :: k) (y :: k) = Sub
  { unSub :: c (x - y)
  } deriving (Eq,Show)
_Sub :: Iso (p (x1 - y1)) (q (x2 - y2)) (Sub p x1 y1) (Sub q x2 y2)
_Sub = iso Sub unSub

newtype Negate (c :: k -> *) (x :: k) = Negate
  { unNegate :: c (Neg x)
  } deriving (Eq,Show)
_Neg :: Iso (p (Neg x)) (q (Neg y)) (Negate p x) (Negate q y)
_Neg = iso Negate unNegate

newtype Div (c :: k -> *) (x :: k) (y :: k) = Div
  { unDiv :: c (x / y)
  } deriving (Eq,Show)
_Div :: Iso (p (x1 / y1)) (q (x2 / y2)) (Div p x1 y1) (Div q x2 y2)
_Div = iso Div unDiv

newtype Reciprocal (c :: k -> *) (x :: k) = Reciprocal
  { unReciprocal :: c (Recip x)
  } deriving (Eq,Show)
_Recip :: Iso (p (Recip x)) (q (Recip y)) (Reciprocal p x) (Reciprocal q y)
_Recip = iso Reciprocal unReciprocal

-- }}}

class NumAdd (n :: k -> *) where
  type N0 n :: k
  (+:)     :: n x -> n y -> n (x + y)
  addIdL   :: n x               -> N0 n + x  :~: x
  addIdR   :: n x               -> x + N0 n  :~: x
  addAssoc :: n x -> n y -> n z -> x + (y + z)   :~: (x + y) + z
  addComm  :: n x -> n y        -> x + y         :~: y + x
infixl 6 +:

class NumAdd n => NumMul (n :: k -> *) where
  type N1 n :: k
  (*:)    :: n x -> n y -> n (x * y)
  mulIdL  :: n x               -> N1 n * x   :~: x
  mulIdR  :: n x               -> x * N1 n   :~: x
  distMul :: n x -> n y -> n z -> x * (y + z)   :~: x * y + x * z
infixl 7 *:

class NumAdd n => NumAddInv (n :: k -> *) where
  neg     :: n x -> n (Neg x)
  (-:)    :: n x -> n y -> n (x - y)
  negNeg  :: n x        -> Neg (Neg x) :~: x
  distNeg :: n x -> n y -> Neg (x + y) :~: Neg x + Neg y
infixl 6 -:

class NumMul n => NumMulInv (n :: k -> *) where
  recip      :: n x -> n (Recip x)
  (/:)       :: n x -> n y -> n (x / y)
  recipRecip :: n x               -> Recip (Recip x) :~: x
  distRecip  :: n x -> n y -> n z -> (y * z) / x     :~: (y / x) * (z / x)
infixl 7 /:

cong :: a :~: b -> f a :~: f b
cong = apply Refl

