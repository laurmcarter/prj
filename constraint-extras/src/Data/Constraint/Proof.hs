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

module Data.Constraint.Proof where

import Data.Constraint.Base
import Data.Type.Equality
import Control.Monad (MonadPlus(..))

class Proof (t :: *) where
  type ProofOf t :: Constraint
  type Residue t :: Constraint
  type Residue t  = EmptyC
  (//) :: Residue t => t -> (ProofOf t => r) -> r
  (\\) :: Residue t => (ProofOf t => r) -> t -> r
  (\\) = flip (//)
  infixr 0 //
  infixl 1 \\

class Proof1 (f :: k -> *) where
  type ProofOf1   f (a :: k) :: Constraint
  type Residue1   f          :: Constraint
  (//.) :: Residue1 f => f a -> (ProofOf1 f a => r) -> r
  (\\.) :: Residue1 f => (ProofOf1 f a => r) -> f a -> r
  (\\.) = flip (//.)
  infixr 0 //.
  infixl 1 \\.

class Proof2 (f :: k -> l -> *) where
  type ProofOf2 f (a :: k) (b :: l) :: Constraint
  type Residue2 f                   :: Constraint
  (//:) :: Residue2 f => f a b -> (ProofOf2 f a b => r) -> r
  (\\:) :: Residue2 f => (ProofOf2 f a b => r) -> f a b -> r
  (\\:) = flip (//:)
  infixr 0 //:
  infixl 1 \\:

class Proof3 (f :: k -> l -> m -> *) where
  type ProofOf3 f (a :: k) (b :: l) (c :: m) :: Constraint
  type Residue3 f                            :: Constraint
  (//:.) :: Residue3 f => f a b c -> (ProofOf3 f a b c => r) -> r
  (\\:.) :: Residue3 f => (ProofOf3 f a b c => r) -> f a b c -> r
  (\\:.) = flip (//:.)
  infixr 0 //:.
  infixl 1 \\:.

instance Proof (Dict c) where
  type ProofOf (Dict c) = c
  Dict // r = r

instance Proof (a :~: b) where
  type ProofOf (a :~: b) = a ~ b
  type Residue (a :~: b) = EmptyC
  Refl // r = r

instance Proof (a :- b) where
  type ProofOf (a :- b) = b
  type Residue (a :- b) = a
  Sub Dict // r = r

instance (Proof a, Proof b) => Proof (a,b) where
  type ProofOf (a,b) = (ProofOf a, ProofOf b)
  type Residue (a,b) = (Residue a, Residue b)
  (a,b) // r = a // b // r

instance Proof (p a b) => Proof (U p '(a,b)) where
  type ProofOf (U p '(a,b)) = ProofOf (p a b)
  type Residue (U p '(a,b)) = Residue (p a b)
  U p // r = p // r

instance Proof (p '(a,b)) => Proof (C p a b) where
  type ProofOf (C p a b) = ProofOf (p '(a,b))
  type Residue (C p a b) = Residue (p '(a,b))
  C p // r = p // r

instance Proof (f (g a)) => Proof ((f .: g) a) where
  type ProofOf ((f .: g) a) = ProofOf (f (g a))
  type Residue ((f .: g) a) = Residue (f (g a))
  Comp t // r = t // r


mproof :: (Proof t, Residue t) => Maybe t -> (ProofOf t => Maybe r) -> Maybe r
mproof mt r = case mt of
  Just t -> t // r
  _      -> Nothing

type family Proofs (f :: k -> *) (as :: [k]) :: Constraint where
  Proofs f '[]       = EmptyC
  Proofs f (a ': as) = (Proof (f a), Proofs f as)

type family ProofsOf (f :: k -> *) (as :: [k]) :: Constraint where
  ProofsOf f '[]       = EmptyC
  ProofsOf f (a ': as) = (ProofOf (f a), ProofsOf f as)

type family Residues (f :: k -> *) (as :: [k]) :: Constraint where
  Residues f '[]       =  EmptyC
  Residues f (a ': as) = (Residue (f a), Residues f as)

