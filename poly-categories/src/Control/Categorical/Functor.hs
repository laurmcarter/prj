{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Categorical.Functor where

import qualified Prelude
import Prelude hiding (id,(.),Functor(..))

import Data.Semigroupoid
import Control.Categorical

class (Category r, Category t)
  => Functor (f :: k -> l) (r :: k -> k -> *) (t :: l -> l -> *)
  | f r -> t, f t -> r where
  fmap :: r a b -> t (f a) (f b)

class    Functor f r r => Endofunctor (f :: k -> k) (r :: k -> k -> *)
instance Functor f r r => Endofunctor f r

newtype LiftedFunctor f a = LiftFunctor
  { unliftFunctor :: f a
  } deriving (Eq,Ord,Read,Show)

newtype LoweredFunctor f a = LowerFunctor
  { unlowerFunctor :: f a
  } deriving (Eq,Ord,Read,Show)

instance Prelude.Functor f => Functor (LiftedFunctor f) (->) (->) where
  fmap f = LiftFunctor . Prelude.fmap f . unliftFunctor

instance Functor f (->) (->) => Prelude.Functor (LoweredFunctor f) where
  fmap f = LowerFunctor . fmap f . unlowerFunctor

instance Functor ((,) a)    (->) (->) where fmap = Prelude.fmap
instance Functor (Either a) (->) (->) where fmap = Prelude.fmap
instance Functor Maybe      (->) (->) where fmap = Prelude.fmap
instance Functor []         (->) (->) where fmap = Prelude.fmap
instance Functor IO         (->) (->) where fmap = Prelude.fmap
instance Functor ((->) r)   (->) (->) where fmap = Prelude.fmap

instance Prelude.Functor f => Prelude.Functor (LiftedFunctor f) where
  fmap = fmap

instance Functor f p (->) => Functor (LoweredFunctor f) p (->) where
  fmap f = LowerFunctor . fmap f . unlowerFunctor


