{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Num.Binary where

import Control.Applicative
import Control.Monad.State
import Data.Bits
import Data.Tuple (swap)
import Data.Proxy
import GHC.Exts (Constraint)

type Bit = Bool

newtype Carry a = Carry
  { unCarry :: State Bit a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadState Bit
  )

{-
-- + {{{

F F F : F F
F F T : F T
F T F : F T
F T T : T F
T F F : F T
T F T : T F
T T F : T F
T T T : T T

-- }}}
-}

carry :: a -> Bit -> Carry a
carry a c = put c >> return a

withCarry :: (Bit -> (Bit,a)) -> Carry a
withCarry f = Carry . state $ swap . f

pattern B0 = False
pattern B1 = True

{-
addBit :: Bit -> Bit -> Carry Bit
addBit x y = withCarry $
  (x `xor` y) `carry` (x .&. y)
-}

{-
addBits :: Vec n Bit -> Vec n Bit -> Carry (Vec n Bit)
addBits x y = case (x,y) of
  (Nil,Nil) -> 
-}

data N
  = Z
  | S N
  deriving (Eq,Show)

data Nat (n :: N) where
  Z_ :: Nat Z
  S_ :: Nat n -> Nat (S n)

deriving instance Eq   (Nat n)
deriving instance Show (Nat n)

data Fin (n :: N) where
  FZ :: Fin (S n)
  FS :: Fin n -> Fin (S n)

deriving instance Eq   (Fin n)
deriving instance Show (Fin n)

data Vec (n :: N) :: * -> * where
  Nil  :: Vec Z a
  (:*) :: KnownNat n => a -> Vec n a -> Vec (S n) a
infixr 4 :*

deriving instance Eq a => Eq (Vec n a)
deriving instance Show a => Show (Vec n a)

{-
instance Bits a => Bits (Vec n a) where
  x .&. y = case (x,y) of
-}

class KnownNat (n :: N) where
  nat    :: Nat n
instance KnownNat Z where
  nat    = Z_
instance KnownNat n => KnownNat (S n) where
  nat    = S_ nat

type family Pred (x :: N) where
  Pred (S n) = n

data Vecs (xs :: [*]) (n :: N) (a :: *) (b :: *) (c :: *) where
  V1 :: Vecs '[x] n a
        ((x,Vec (Pred n) x) -> a)
        (Vec n x      -> a)
  VS :: Vecs xs n a b c
     -> Vecs (x ': xs) n  a
        ((x,Vec (Pred n) x) -> b)
        (Vec n x      -> c)

caseVec :: Vecs xs n a b c
  -> (n ~ Z          => a)
  -> (n ~ S (Pred n) => b)
  -> c
caseVec vs z s = case vs of
  V1 -> \case
    Nil     -> z
    a :* as -> s (a,as)
  VS vs' -> \v -> caseVec vs' z $
    case v of
      a :* as -> s (a,as)

allEq_ :: Eq a => Vec n a -> Vec n a -> Bool
allEq_ = caseVec (VS V1) True $
  \(a1,v1) (a2,v2) -> a1 == a2 && allEq_ v1 v2

allEq :: Eq a => Vec n a -> Vec n a -> Maybe (a,a,Fin n)
allEq = caseVec (VS V1) Nothing $ \(a1,v1) (a2,v2) ->
  if a1 == a2
    then fmap (\(c1,c2,n) -> (c1,c2,FS n)) $ allEq v1 v2
    else Just (a1,a2,FZ)

type family AllSame (as :: [*]) :: Constraint where
  AllSame '[]       = ()
  AllSame (a ': as) = as :~ a

type family (as :: [*]) :~ (a :: *) :: Constraint where
  '[]        :~ a = ()
  (a' ': as) :~ a = (a' ~ a, as :~ a)

v0 :: Vec (S (S Z)) Int
v0 = 0 :* 1 :* Nil

v1 :: Vec (S (S Z)) Int
v1 = 0 :* 2 :* Nil

