{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}

module Data.Constraint.Extras where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Complex
import Data.Monoid
import Data.Ratio
import Data.Constraint (Dict(..),Constraint)
import qualified Data.Constraint as C

newtype a :-- b = Sub_ (Dict a -> Dict b)

instance Eq (a :-- b) where
  _ == _ = True

instance Ord (a :-- b) where
  compare _ _ = EQ

instance Show (a :-- b) where
  showsPrec d _ = showParen (d > 10) $ showString "Sub Dict"

(\\) :: a => (b => r) -> (a :-- b) -> r
r \\ Sub_ f = case f Dict of
  Dict -> r
infixl 1 \\

weaken1 :: (a,b) :-- a
weaken1 = Sub_ $ \Dict -> Dict

weaken2 :: (a,b) :-- b
weaken2 = Sub_ $ \Dict -> Dict

contract :: a :-- (a,a)
contract = Sub_ $ \Dict -> Dict

(&&&) :: (a :-- b) -> (a :-- c) -> (a :-- (b,c))
f &&& g = Sub_ $ \Dict -> Dict \\ f \\ g

(***) :: (a :-- b) -> (c :-- d) -> (a,c) :-- (b,d)
f *** g = Sub_ $ \Dict -> Dict \\ f \\ g

refl :: a :-- a
refl = Sub_ id

trans :: (b :-- c) -> (a :-- b) -> (a :-- c)
trans f g = Sub_ $ \Dict -> Dict \\ f \\ g

top :: a :-- ()
top = Sub_ $ \Dict -> Dict

type family Ex (a :: *) (c :: Constraint) :: Constraint
type instance Ex ()   c = ()
type instance Ex Bool c = c

falso :: (() ~ a) :-- Ex a c
falso = Sub_ $ \Dict -> Dict

bottom :: (() ~ Bool) :-- c
bottom = falso

class Class b h | h -> b where
  cls :: h :-- b

class b :=> h | h -> b where
  ins   :: b :-- h
  super :: Dict h -> Dict b
infixr 9 :=>

instance Class () (Class b a) where cls = Sub_ $ \Dict -> Dict
instance Class () (b :=> h)   where cls = Sub_ $ \Dict -> Dict
instance Class b a => () :=> Class b a where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance (b :=> a) => () :=> (b :=> a) where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict

-- Eq
instance Class () (Eq a) where cls = Sub_ $ \Dict -> Dict
instance () :=> Eq () where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Eq Int where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Eq Bool where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Eq Integer where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Eq Float where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Eq Double where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance Eq a :=> Eq [a] where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance Eq a :=> Eq (Maybe a) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance Eq a :=> Eq (Complex a) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance Eq a :=> Eq (Ratio a) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance (Eq a, Eq b) :=> Eq (a, b) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance (Eq a, Eq b) :=> Eq (Either a b) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance () :=> Eq (Dict a) where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Eq (a :-- b) where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict

-- Ord
instance Class (Eq a) (Ord a) where cls = Sub_ $ \Dict -> Dict
instance () :=> Ord () where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Ord Bool where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Ord Int where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance ():=> Ord Integer where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Ord Float where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance ():=> Ord Double where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Ord Char where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance Ord a :=> Ord (Maybe a) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance Ord a :=> Ord [a] where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance (Ord a, Ord b) :=> Ord (a, b) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance (Ord a, Ord b) :=> Ord (Either a b) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance Integral a :=> Ord (Ratio a) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance () :=> Ord (Dict a) where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Ord (a :-- b) where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict

instance Class () (Show a) where cls = Sub_ $ \Dict -> Dict
instance () :=> Show () where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Show Bool where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Show Ordering where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Show Char where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance Show a :=> Show (Complex a) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance Show a :=> Show [a] where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance Show a :=> Show (Maybe a) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance (Show a, Show b) :=> Show (a, b) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance (Show a, Show b) :=> Show (Either a b) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance (Integral a, Show a) :=> Show (Ratio a) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance () :=> Show (Dict a) where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Show (a :-- b) where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict

instance Class () (Read a) where cls = Sub_ $ \Dict -> Dict
instance () :=> Read () where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Read Bool where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Read Ordering where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Read Char where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance Read a :=> Read (Complex a) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance Read a :=> Read [a] where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance Read a :=> Read (Maybe a) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance (Read a, Read b) :=> Read (a, b) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance (Read a, Read b) :=> Read (Either a b) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance (Integral a, Read a) :=> Read (Ratio a) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict

instance Class () (Enum a) where cls = Sub_ $ \Dict -> Dict
instance () :=> Enum () where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Enum Bool where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Enum Ordering where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Enum Char where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Enum Int where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Enum Integer where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Enum Float where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Enum Double where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance Integral a :=> Enum (Ratio a) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict

instance Class () (Bounded a) where cls = Sub_ $ \Dict -> Dict
instance () :=> Bounded () where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Bounded Ordering where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Bounded Bool where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Bounded Int where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Bounded Char where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance (Bounded a, Bounded b) :=> Bounded (a,b) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict

instance Class () (Num a) where cls = Sub_ $ \Dict -> Dict
instance () :=> Num Int where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Num Integer where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Num Float where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Num Double where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance RealFloat a :=> Num (Complex a) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance Integral a :=> Num (Ratio a) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict

instance Class (Num a, Ord a) (Real a) where cls = Sub_ $ \Dict -> Dict
instance () :=> Real Int where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Real Integer where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Real Float where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Real Double where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance Integral a :=> Real (Ratio a) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict

instance Class (Real a, Enum a) (Integral a) where cls = Sub_ $ \Dict -> Dict
instance () :=> Integral Int where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Integral Integer where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict

instance Class (Num a) (Fractional a) where cls = Sub_ $ \Dict -> Dict
instance () :=> Fractional Float where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Fractional Double where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance RealFloat a :=> Fractional (Complex a) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance Integral a :=> Fractional (Ratio a) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict

instance Class (Fractional a) (Floating a) where cls = Sub_ $ \Dict -> Dict
instance () :=> Floating Float where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Floating Double where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance RealFloat a :=> Floating (Complex a) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict

instance Class (Real a, Fractional a) (RealFrac a) where cls = Sub_ $ \Dict -> Dict
instance () :=> RealFrac Float where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> RealFrac Double where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance Integral a :=> RealFrac (Ratio a) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict

instance Class (RealFrac a, Floating a) (RealFloat a) where cls = Sub_ $ \Dict -> Dict
instance () :=> RealFloat Float where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> RealFloat Double where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict

instance Class () (Monoid a) where cls = Sub_ $ \Dict -> Dict
instance () :=> Monoid () where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Monoid Ordering where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Monoid [a] where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance Monoid a :=> Monoid (Maybe a) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance (Monoid a, Monoid b) :=> Monoid (a, b) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict

instance Class () (Functor f) where cls = Sub_ $ \Dict -> Dict
instance () :=> Functor [] where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Functor Maybe where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Functor (Either a) where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Functor ((->) a) where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Functor ((,) a) where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Functor IO where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance Monad m :=> Functor (WrappedMonad m) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict

instance Class (Functor f) (Applicative f) where cls = Sub_ $ \Dict -> Dict
instance () :=> Applicative [] where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Applicative Maybe where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Applicative (Either a) where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Applicative ((->)a) where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Applicative IO where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance Monoid a :=> Applicative ((,)a) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance Monad m :=> Applicative (WrappedMonad m) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict

instance Class (Applicative f) (Alternative f) where cls = Sub_ $ \Dict -> Dict
instance () :=> Alternative [] where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Alternative Maybe where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance MonadPlus m :=> Alternative (WrappedMonad m) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict

instance Class () (Monad f) where cls = Sub_ $ \Dict -> Dict
instance () :=> Monad [] where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Monad ((->) a) where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Monad (Either a) where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> Monad IO where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict

instance Class (Monad f) (MonadPlus f) where cls = Sub_ $ \Dict -> Dict
instance () :=> MonadPlus [] where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict
instance () :=> MonadPlus Maybe where
  ins = Sub_ $ \Dict -> Dict
  super Dict = Dict

-- UndecidableInstances
instance a :=> Enum (Dict a) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance a :=> Bounded (Dict a) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance a :=> Read (Dict a) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict
instance a :=> Monoid (Dict a) where
  ins = Sub_ $ \Dict -> Dict
  -- super Dict = Dict

