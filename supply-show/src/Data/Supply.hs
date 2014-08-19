{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Supply where

import Data.Type.Equality
import Data.List (intersperse)
import GHC.Exts (Any)

class Supply (g :: *) where
  type Fresh g :: *
  split :: g -> (Fresh g,g)
  newSupply :: g
  supplyAvail :: g -> Bool
  takeSupply :: Int -> g -> ([Fresh g],g)
  dropSupply :: Int -> g -> g

instance Supply Integer where
  type Fresh Integer = Integer
  split i   = (i,succ i)
  newSupply = 0
  supplyAvail _ = True
  takeSupply n i = ([i..j],succ j)
    where
    j = i + (toEnum n) - 1
  dropSupply n i = toEnum n + i

newtype Vars = Vars
  { getVars :: [String]
  }

instance Show Vars where
  showsPrec d (Vars vs) = showParen (d > 10)
    $ showString "Vars ["
    . compose xs
    . showString "...]"
    where
    xs = map ((. showString ", ") . shows) $ take 5 vs

instance Supply Vars where
  type Fresh Vars = String
  split (Vars vs) = case vs of
    v:vs' -> (v,Vars vs')
    _     -> error "Empty supply"
  newSupply = Vars
       $ vs
    ++ [ v ++ show i
       | i <- [(0 :: Integer)..]
       , v <- vs 
       ]
    where
    vs = map (:[]) ['a'..'z']
  supplyAvail = not . null . getVars
  takeSupply n (Vars vs) = (xs,Vars vs')
    where
    (xs,vs') = splitAt n vs
  dropSupply n (Vars vs) = Vars $ drop n vs

class Supply1 (g :: k -> *) where
  type Fresh1     g (a :: k) :: *
  type Next1      g (a :: k) :: k
  type NewSupply1 g          :: k
  split1 :: g a -> (Fresh1 g a, g (Next1 g a))
  newSupply1 :: g (NewSupply1 g)

-- List {{{

data List (f :: k -> *) :: [k] -> * where
  Nil  :: List f '[]
  (:*) :: { lhead :: f a
          , ltail :: List f as
          } -> List f (a ': as)
infixr 4 :*

-- }}}

-- Stream {{{

data Stream (p :: k -> *) :: (k -> k) -> k -> * where
  (:>) :: { shead :: p z
          , stail :: Stream p s (s z)
          } -> Stream p s z
infixr 4 :>

sIncr :: (PNat p, s ~ PSucc p) => Stream p s z -> Stream p s (s z)
sIncr (z :> s) = (princSucc z // tsucc z) :> sIncr s

instance (PNat p, s ~ PSucc p) => Supply1 (Stream (p :: k -> *) (s :: k -> k)) where
  type Fresh1     (Stream p s) x = p x
  type Next1      (Stream p s) x = s x
  type NewSupply1 (Stream p s)   = Zero p
  split1 (z :> s) = (z,s)
  newSupply1 = tzero :> sIncr newSupply1

-- }}}

-- TNat / TMonoid {{{

class TNat (p :: k -> *) where
  type Zero p          :: k
  type Succ p (x :: k) :: k
  tzero :: p (Zero p)
  tsucc :: p x -> p (Succ p x)

class TNat p => PNat (p :: k -> *) where
  type PSucc p :: k -> k
  princSucc :: p x -> Succ p x :~: PSucc p x

class TMonoid (p :: k -> *) where
  type Mempty  p :: k
  type Mappend p (a :: k) (b :: k) :: k
  tmempty  :: p (Mempty p)
  tmappend :: p a -> p b -> p (Mappend p a b)

-- }}}

-- Nat {{{

data N
  = Z
  | S N
  deriving (Eq,Show)

data Nat :: N -> * where
  Z_ :: Nat Z
  S_ :: Nat n -> Nat (S n)

deriving instance Eq   (Nat x)
deriving instance Show (Nat x)

instance TNat Nat where
  type Zero Nat   = Z
  type Succ Nat x = S x
  tzero = Z_
  tsucc = S_

instance PNat Nat where
  type PSucc Nat = S
  princSucc _ = Refl

type family (x :: N) + (y :: N) :: N where
  Z   + y = y
  S x + y = S (x + y)
infixl 6 +

(+:) :: Nat x -> Nat y -> Nat (x + y)
x +: y = case x of
  Z_    -> y
  S_ x' -> S_ $ x' +: y
infixl 6 +:

type family (x :: N) * (y :: N) :: N where
  Z   * y = Z
  S x * y = y + x * y
infixl 7 *

(*:) :: Nat x -> Nat y -> Nat (x * y)
x *: y = case x of
  Z_    -> Z_
  S_ x' -> y +: x' *: y
infixl 7 *:

-- }}}

-- Sum {{{

newtype Sum (x :: N) = Sum
  { runSum :: Nat x
  } deriving (Eq,Show)

instance TNat Sum where
  type Zero Sum   = Z
  type Succ Sum x = S x
  tzero = Sum tzero
  tsucc = Sum . tsucc . runSum

instance PNat Sum where
  type PSucc Sum = S
  princSucc _ = Refl

instance TMonoid Sum where
  type Mempty  Sum     = Z
  type Mappend Sum x y = x + y
  tmempty = tzero
  tmappend (Sum x) (Sum y) = Sum $ x +: y

-- }}}

-- Prod {{{

newtype Prod (x :: N) = Prod
  { runProd :: Nat x
  } deriving (Eq,Show)

instance TNat Prod where
  type Zero Prod   = Z
  type Succ Prod x = S x
  tzero = Prod tzero
  tsucc = Prod . tsucc . runProd

instance PNat Prod where
  type PSucc Prod = S
  princSucc _ = Refl

instance TMonoid Prod where
  type Mempty  Prod     = Z
  type Mappend Prod x y = x * y
  tmempty = tzero
  tmappend (Prod x) (Prod y) = Prod $ x *: y

-- }}}

(//) :: a :~: b -> (a ~ b => r) -> r
Refl // r = r
infixr 1 //

compose :: [a -> a] -> a -> a
compose = foldr (.) id

