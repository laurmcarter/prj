{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module Data.Type.Monoid where

import Data.Type.Defunction
import Data.Type.Bool
import Data.Function (on)

-- Monoid {{{

data Mempty  :: F (k -> *) k -> *
data Mappend :: F (k -> *) (F k (F k k -> *) -> *) -> *

class TMonoid (p :: k -> *) where
  tmempty  ::               p (Mempty  $ p)
  tmappend :: p a -> p b -> p (Mappend $ p $ a $ b)

-- }}}

-- Dual {{{

newtype Dual (f :: k -> *) (x :: k) = Dual
  { getDual :: f x
  }

type instance  Mempty   @: Dual f            = Mempty  $ f
type instance (Mappend :@  Dual f :@ a) @: b = Mappend $ f $ b $ a

{-
instance TMonoid f => TMonoid (Dual f) where
  tmempty = Dual _
-}

-- }}}

-- List {{{

data List (f :: k -> *) (as :: [k]) where
  Nil  :: List f '[]
  (:*) :: f a -> List f as -> List f (a ': as)
infixr 4 :*

type instance  Mempty   @: List f            = '[]
type instance (Mappend :@  List f :@ a) @: b = a ++ b

type family (as :: [k]) ++ (bs :: [k]) :: [k] where
  '[]       ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

(++:) :: List f as -> List f bs -> List f (as ++ bs)
(++:) = tmappend
infixr 5 ++:

instance TMonoid (List (f :: k -> *)) where
  tmempty = Nil
  tmappend as bs = case as of
    Nil -> bs
    a :* as' -> a :* as' ++: bs

-- }}}


-- Nat {{{

data N
  = Z
  | S N

data Nat (x :: N) where
  Z_ :: Nat Z
  S_ :: Nat x -> Nat (S x)

-- }}}

-- Addition {{{

newtype NSum (x :: N) = NSum
  { nSum :: Nat x
  }

type instance  Mempty   @: NSum            = Z
type instance (Mappend :@  NSum :@ x) @: y = x + y

type family (x :: N) + (y :: N) :: N where
  Z    + y = y
  S x' + y = S (x' + y)
infixl 6 +

instance TMonoid NSum where
  tmempty = NSum Z_
  tmappend = underT nSum NSum $ \x y -> case x of
    Z_    -> y
    S_ x' -> S_ (x' +: y)
  {-# INLINE tmappend #-}

(+:) :: Nat x -> Nat y -> Nat (x + y)
(+:) = underT NSum nSum tmappend
infixl 6 +:
{-# INLINE (+:) #-}

-- }}}

-- Multiplication {{{

newtype NProd (x :: N) = NProd
  { nProd :: Nat x
  }

type instance  Mempty   @: NProd            = S Z
type instance (Mappend :@  NProd :@ x) @: y = x * y

type family (x :: N) * (y :: N) :: N where
  Z   * y = Z
  S x * y = y + (x * y)
infixl 7 *

instance TMonoid NProd where
  tmempty = NProd $ S_ Z_
  tmappend = underT nProd NProd $ \x y -> case x of
    Z_    -> Z_
    S_ x' -> y +: (x' *: y)

(*:) :: Nat x -> Nat y -> Nat (x * y)
(*:) = underT NProd nProd tmappend
infixl 7 *:

-- }}}


-- Bool {{{

data Boolean (x :: Bool) where
  True_  :: Boolean True
  False_ :: Boolean False

-- }}}

-- Conjunction {{{

newtype BAll (x :: Bool) = BAll
  { bAll :: Boolean x
  }

type instance  Mempty   @: BAll            = True
type instance (Mappend :@  BAll :@ x) @: y = x && y

instance TMonoid BAll where
  tmempty = BAll True_
  tmappend = underT bAll BAll $ \x y -> case x of
    True_  -> y
    False_ -> False_

(&&:) :: Boolean x -> Boolean y -> Boolean (x && y)
(&&:) = underT BAll bAll tmappend
infixr 3 &&:

-- }}}

-- Disjunction {{{

newtype BAny (x :: Bool) = BAny
  { bAny :: Boolean x
  }

type instance  Mempty   @: BAny            = False
type instance (Mappend :@  BAny :@ x) @: y = x || y

instance TMonoid BAny where
  tmempty  = BAny False_
  tmappend = underT bAny BAny $ \x y -> case x of
    True_  -> True_
    False_ -> y

(||:) :: Boolean x -> Boolean y -> Boolean (x || y)
(||:) = underT BAny bAny tmappend
infixr 2 ||:

-- }}}

