{-# LANGUAGE OverlappingInstances #-}
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

module Data.Constraint.Biconditional where

import Data.Constraint.Base
import qualified Data.Constraint as C ((***),(&&&))
import Data.Constraint.Proof
import Data.Constraint.Product
import Data.Constraint.Sum

import Control.Arrow
import Control.Category
import Prelude hiding (id,(.))
import Data.Type.Bool
import Data.Proxy

-- | Captures the constraint structure of two
--   constraints which each hold iff the other holds.
data (a :: Constraint) -:- (b :: Constraint) = Bi
  { biTo   :: a :- b
  , biFrom :: b :- a
  } deriving (Eq,Ord,Show)

instance Proof (a -:- b) where
  type ProofOf (a -:- b) = b
  type Residue (a -:- b) = a
  Bi (Sub Dict) _ // r = r
  r \\ Bi (Sub Dict) _ = r

biRefl :: a -:- a
biRefl = Bi id id

biSym :: (a -:- b) -> (b -:- a)
biSym ab = Bi
  { biTo   = biFrom ab
  , biFrom = biTo   ab
  }

biTrans :: (b -:- c) -> (a -:- b) -> (a -:- c)
biTrans bc ab = Bi
  { biTo   = biTo   bc . biTo   ab
  , biFrom = biFrom ab . biFrom bc
  }

instance Category (-:-) where
  id  = biRefl
  (.) = biTrans

(***) :: (a -:- b) -> (c -:- d) -> (a,c) -:- (b,d)
ab *** cd = Bi
  { biTo   = biTo ab   C.*** biTo cd
  , biFrom = biFrom ab C.*** biFrom cd
  }

{-
(&&&) :: (a -:- b) -> (a -:- c) -> (a -:- (b,c))
ab &&& ac = Bi
  { biTo   = biTo ab C.&&& biTo ac
  , biFrom = _
  }
-}

{-
class (Duals t u :+: Duals u t) => HasDual t u

type instance Holds (Duals (a :- b) (b' :- a'))
  = Holds (Duals a a') && Holds (Duals b b')

type instance Holds (Duals (a,b) (Either b' a'))
  = Holds (Duals a a') && Holds (Duals b b')

type instance Holds (Duals a b) = DualsHoldC a b

type family DualsHoldC (a :: Constraint) (b :: Constraint) :: Bool where
  DualsHoldC  (a :*: b)     (b' :+: a')    = DualsHoldC a a' && DualsHoldC b b'

class Duals (t :: k) (u :: k) | t -> u, u -> t where
instance (Duals a a', Duals b b') => Duals  (a :- b)      (b' :- a')
instance (Duals a a', Duals b b') => Duals  (a -:- b)     (b' -:- a')
instance (Duals a a', Duals b b') => Duals  (a,b)         (Either b a)
instance (Duals a a', Duals b b') => Duals  (a :*: b)     (b' :+: a')
-- XXX?
instance
  ( Duals a a'
  , Duals b b'
  , Duals c c'
  ) => Duals ((a :.: b) c) ((b' :.: a') c')
-}

