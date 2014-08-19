{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Constraint.Dict where

import Data.Constraint.Base
import Data.Constraint.Proof

data Dict1 (f :: k -> Constraint) (a :: k) where
  Dict1 :: f a => Dict1 f a

data Dict2 (f :: k -> l -> Constraint) (a :: k) (b :: l) where
  Dict2 :: f a b => Dict2 f a b

data Dict3 (f :: k -> l -> m -> Constraint) (a :: k) (b :: l) (c :: m) where
  Dict3 :: f a b c => Dict3 f a b c


instance Proof (Dict1 f a) where
  type ProofOf (Dict1 f a) = f a
  type Residue (Dict1 f a) = EmptyC
  Dict1 // r = r

instance Proof (Dict2 f a b) where
  type ProofOf (Dict2 f a b) = f a b
  type Residue (Dict2 f a b) = EmptyC
  Dict2 // r = r

instance Proof (Dict3 f a b c) where
  type ProofOf (Dict3 f a b c) = f a b c
  type Residue (Dict3 f a b c) = EmptyC
  Dict3 // r = r


instance Proof1 (Dict1 f) where
  type ProofOf1 (Dict1 f) a = f a
  type Residue1 (Dict1 f)   = EmptyC
  Dict1 //. r = r

instance Proof1 (Dict2 f a) where
  type ProofOf1 (Dict2 f a) b = f a b
  type Residue1 (Dict2 f a)   = EmptyC
  Dict2 //. r = r

instance Proof1 (Dict3 f a b) where
  type ProofOf1 (Dict3 f a b) c = f a b c
  type Residue1 (Dict3 f a b)   = EmptyC
  Dict3 //. r = r


instance Proof2 (Dict2 f) where
  type ProofOf2 (Dict2 f) a b = f a b
  type Residue2 (Dict2 f)     = EmptyC
  Dict2 //: r = r

instance Proof2 (Dict3 f a) where
  type ProofOf2 (Dict3 f a) b c = f a b c
  type Residue2 (Dict3 f a)     = EmptyC
  Dict3 //: r = r


instance Proof3 (Dict3 f) where
  type ProofOf3 (Dict3 f) a b c = f a b c
  type Residue3 (Dict3 f)       = EmptyC
  Dict3 //:. r = r


-- | IsDict provides a way of freely moving back and forth
--   between the traditional 'Dict' of kind (Constraint -> *)
--   and the higher order Dict types, of kinds such as
--   ((k -> Constraint) -> k -> *), ((k -> l -> Constraint) -> k -> l -> *),
--   etc.
--   An instance of 'IsDict' requires that the type can be used to introduce
--   facts to the typechecker (via the 'Proof' class), and that its use does
--   not incur any residual constraints.
class (Proof t, Residue t ~ EmptyC) => IsDict (t :: *) where
  toDict   :: t -> Dict (ProofOf t)
  fromDict :: Dict (ProofOf t) -> t

instance IsDict (Dict1 f a) where
  toDict   Dict1 = Dict
  fromDict Dict  = Dict1

instance IsDict (Dict2 f a b) where
  toDict   Dict2 = Dict
  fromDict Dict  = Dict2

instance IsDict (Dict3 f a b c) where
  toDict   Dict3 = Dict
  fromDict Dict  = Dict3

