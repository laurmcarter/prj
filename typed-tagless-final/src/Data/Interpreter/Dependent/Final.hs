{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Data.Interpreter.Dependent.Final where

import Data.Proxy
import GHC.Exts (Constraint,Any)

class Dependent r where
  λ     :: (r i t a -> r j t' b) -> r (S (Max i j)) (Pi mab t t') (a -> b)
  app   :: r i (Pi j k mab t t') (a -> b) -> r j t a -> r k t' b
  star  :: r (S i) Star Star
  -- π     :: (r t a -> r Star t -> r Star t') -> r Star (Pi (Just '(a,b)) t t')
  -- (-->) :: r Star t -> r Star t' -> r Star (Pi Nothing t t')
  -- (∷)   :: r t a -> r Star t -> r t a

data Z_
data S_ n

data Star = Star
  deriving (Eq,Show)

data Pi i j mab t t' = forall r a b.
  ( Dependent r
  , UnifyM mab a b
  ) => Pi
  { unPi :: r i t a -> r j t' b
  }

type family UnifyM (mab :: Maybe (*,*)) (a :: *) (b :: *) :: Constraint where
  UnifyM Nothing a b         = ()
  UnifyM (Just '(a,b)) a' b' = (a ~ a',b ~ b')

type family SubstM (ma :: Maybe (*,*)) (r :: *) (t :: k) :: k where
  SubstM Nothing r b       = b
  SubstM (Just '(a,c)) r b = Subst a r b

type family Subst (a :: *) (r :: *) (t :: k) :: k where
  Subst a r a            = r
  Subst a r (f (b :: *)) = Subst a r f (Subst a r b)
  Subst a r b            = b

newtype R t a = R { eval_ :: a }

{-
  λ     :: UnifyM mab a b => (r t a -> r t' b) -> r (Pi mab t t') (a -> b)
  app   :: UnifyM mab a b => r (Pi mab t t') (a -> b) -> r t a -> r t' b
  star  :: r (Star (S i)) (Star i)
  π     :: (r t a -> r (Star i) t -> r (Star j) t') -> r (Star (S (Max i j))) (Pi (Just '(a,b)) t t')
  (-->) :: r (Star i) t -> r (Star j) t' -> r (Star (S (Max i j))) (Pi Nothing t t')
  (∷)   :: r t a -> r (Star i) t -> r t a
-}






{-
class Natural (r :: [*] -> [*] -> * -> * -> *) where
  nz  :: r ht hv Nat (V Z)
  ns  :: r ht hv Nat (V n) -> r ht hv Nat (V (S n))
  nat :: r ht hv (Star i) Nat

class Boolean (r :: [*] -> [*] -> * -> * -> *) where
  tt   :: r ht hv Bool (V True )
  ff   :: r ht hv Bool (V False)

data Star i = Star { universe :: V i }

data Pi ma t t' = forall r a b.
  ( Dependent r
  , UnifyM ma a
  ) => Pi
  { unPi :: r '[] '[] t a -> Scope r ma t t' b
  }

type family UnifyM (ma :: Maybe *) (b :: *) :: Constraint where
  UnifyM Nothing  b = ()
  UnifyM (Just a) b = a ~ b

type family Scope r (ma :: Maybe *) (t :: *) :: * -> * -> * where
  Scope r Nothing  t = r '[] '[]
  Scope r (Just a) t = r '[t] '[a]
-}



data family V (x :: k) :: *

class Known (x :: k) where
  known :: V x

-- Nat {{{

data Nat
  = Z
  | S Nat
  deriving (Eq,Show)

type N (x :: Nat) = V x

data instance V (x :: Nat) where
  Z_ :: V Z
  S_ :: V n -> V (S n)

instance Known Z where
  known = Z_

instance Known n => Known (S n) where
  known = S_ known

type family Max (i :: Nat) (j :: Nat) :: Nat where
  Max  Z     Z    = Z
  Max (S x)  Z    = S x
  Max  Z    (S y) = S y
  Max (S x) (S y) = S (Max x y)

maxNat :: V i -> V j -> V (Max i j)
maxNat i j = case (i,j) of
  (Z_,Z_) -> Z_
  (Z_,S_ _) -> j
  (S_ _,Z_) -> i
  (S_ i',S_ j') -> S_ $ maxNat i' j'

-- }}}

-- Bool {{{

data instance V (x :: Bool) where
  True_  :: V True
  False_ :: V False

instance Known True where
  known = True_

instance Known False where
  known = False_

-- }}}

data instance V (x :: Maybe *) where
  Nothing_ :: V (Nothing :: Maybe *)
  Just_    :: x -> V (Just x)

type List xs = V (xs :: [*])

data instance V (x :: [*]) where
  Nil :: V ('[] :: [*])
  (:*) :: x -> V xs -> V (x ': xs)
infixr 4 :*


