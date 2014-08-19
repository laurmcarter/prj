{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Type.Application where

import Data.Proxy

data N
  = Z
  | S N
  deriving (Eq,Show)

data V (x :: N)
-- data F x

-- Application {{{

class CanApply f x r (p :: Bool) | f x -> r p

instance (A f x r, p ~ True) => CanApply f x r p
instance (p ~ False)         => CanApply f x r p

class
  ( HasType x , HasType r
  , f ∷ (TypeOf x -> TypeOf r)
  ) => A f x r | f x -> r

testA :: A f x r => Proxy f -> Proxy x -> Proxy r
testA _ _ = Proxy

instance
  ( tf ~ TypeOf f
  , Saturated tf p
  , A' p f x r
  ) => A f x r

class
  ( HasType x , HasType r
  , f ∷ (TypeOf x -> TypeOf r)
  ) => A' (p :: Bool) f x r | p f x -> r

testA' :: A' p f x r => Proxy p -> Proxy f -> Proxy x -> Proxy r
testA' _ _ _ = Proxy

instance
  ( HasType x
  , f ∷ (TypeOf x -> r)
  ) => A' False f x (f $ x)

class 
  ( HasType x , HasType r
  , f ∷ (TypeOf x -> TypeOf r)
  ) => Define f x r | f x -> r

testDefine :: Define f x r => Proxy f -> Proxy x -> Proxy r
testDefine _ _ = Proxy

instance Define f x r => A' True f x r

-- }}}

-- Closure building {{{

data (f :: *) $ x
infixl 1 $

type family App (a :: *) (b :: *) :: * where
  App (x -> r) x = r

testApp :: Proxy a -> Proxy b -> Proxy (App a b)
testApp _ _ = Proxy

-- }}}

-- Evaluation {{{

{-
class E (a :: k) (b :: k) | a -> b

-- Bool
instance E (x :: Bool) x

testBool :: E b r => Proxy (b :: Bool) -> Proxy r
testBool _ = Proxy

-- N
instance E (x :: N) x

-- abstraction
instance E (F x) (F x)

-- application
instance
  ( E x x'
  , A (F f) x' r
  ) => E (F f $ x) r
instance
  ( E (f  $ x) r' 
  , E (r' $ y) r
  ) => E (f $ x $ y) r
-}

class                 IsValue (t :: k) (r :: Bool) | t -> r
instance r ~ True  => IsValue (x :: Bool) r
instance r ~ True  => IsValue (x :: N)    r
instance r ~ False => IsValue x           r

testValue :: IsValue t r => Proxy t -> Proxy r
testValue _ = Proxy

-- }}}

-- Typing {{{

class HasType (x :: k) where
  type TypeOf x :: *

testType :: HasType x => Proxy x -> Proxy (TypeOf x)
testType _ = Proxy

instance HasType (x :: Bool) where
  type TypeOf x = Bool

instance HasType (x :: N) where
  type TypeOf x = N

type a ∷ t = (HasType a, TypeOf a ~ t)

{-
instance HasType x => HasType (F x) where
  type TypeOf (F x) = TypeOf x
-}

instance
  ( HasType x
  , f ∷ (TypeOf x -> r)
  ) => HasType (f $ x) where
  type TypeOf (f $ x) = App (TypeOf f) (TypeOf x)

-- }}}

class    Saturated (t :: *) (r :: Bool) | t -> r
instance              Saturated (a -> b -> c) False
instance r ~ True  => Saturated a             r

testSaturated :: Saturated t r => Proxy t -> Proxy r
testSaturated _ = Proxy

typeSaturated :: (tf ~ TypeOf f, Saturated tf r) => Proxy f -> Proxy r
typeSaturated _ = Proxy

-- Boolean Functions {{{

{-
type TestB x = E (And $ (Not $ x) $ (Not $ (Not $ x))) r => r

type Test0   = forall f r. E True r => f r
type Test1 x = E (F Not $ x) r => r
-}

data Not
instance HasType Not where
  type TypeOf Not = Bool -> Bool

instance Define Not True  False
instance Define Not False True

data And
instance HasType And where
  type TypeOf And = Bool -> Bool -> Bool

instance (x ∷ Bool) => Define (And $ False) x False
instance (x ∷ Bool) => Define (And $  True) x x

-- }}}

{-
-- Var typing rules {{{

instance HasType h a => HasType (a :* h) (V Z) where
  type TypeOf (a :* h) (V Z) = TypeOf h a

instance HasType h (V n) => HasType (x :* h) (V (S n)) where
  type TypeOf (x :* h) (V (S n)) = TypeOf h (V n)

-- Env type
data (a :: k) :* (b :: *)
infixr 4 :*

-- }}}
-}

