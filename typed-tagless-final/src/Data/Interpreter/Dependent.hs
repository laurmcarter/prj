{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Data.Interpreter.Dependent where

import Data.Functor.Identity

class Dependent (r :: * -> * -> *) where
  z    :: r (a,h) a
  s    :: r h a -> r (x,h) a
  λ    :: r (a,h) b -> r h (a -> b)
  (#)  :: r h (a -> b) -> r h a -> r h b
  π    :: r h (Type (a,h') a) -> r (Type (a,h') a,h) (Type (a,h') b)
       -> r h (Type h' (a -> b))
  (∷)  :: r h a -> r h (Type h' a) -> r h a
  star :: r h (Type h' a)

newtype E h a = E { eval_ :: h -> a }

instance Dependent E where
  z      = E $ \(a,_) -> a
  s v    = E $ \(_,h) -> eval_ v h
  λ body = E $ \h -> \a -> eval_ body (a,h)
  f # x  = E $ \h -> eval_ f h $ eval_ x h
  π (t :: E h (Type (a,h') a)) (t' :: E (Type (a,h') a,h) (Type (a,h') b)) = E $ \h ->
    let t1 = eval_ t h       :: Type (a,h') a
        t2 = eval_ t' (t1,h)
    in undefined -- λ t2
  e ∷ t  = E $ \h -> eval_ e h
  star   = E $ \h -> eval_ star h

type Type h a = forall r. Dependent r => r h a

eval :: E () a -> a
eval = ($ ()) . eval_

type Def a = forall r. Dependent r => r () a

id' :: Def (a -> a)
id' = λ z

const' :: Def (a -> b -> a)
const' = λ $ λ $ s z

-- idT () = π star z

{-
-- Nat {{{

data N
  = Z
  | S N
  deriving (Eq,Show)

data Nat (n :: N) where
  Z_ :: Nat Z
  S_ :: Nat n -> Nat (S n)

class KnownNat (n :: N) where
  nat :: Nat n

instance KnownNat Z where
  nat = Z_

instance KnownNat n => KnownNat (S n) where
  nat = S_ nat

type family Max (i :: N) (j :: N) :: N where
  Max  Z     Z    = Z
  Max (S x)  Z    = S x
  Max  Z    (S y) = S y
  Max (S x) (S y) = S (Max x y)

maxNat :: Nat i -> Nat j -> Nat (Max i j)
maxNat i j = case (i,j) of
  (Z_,Z_) -> Z_
  (Z_,S_ _) -> j
  (S_ _,Z_) -> i
  (S_ i',S_ j') -> S_ $ maxNat i' j'

-- }}}
-}

