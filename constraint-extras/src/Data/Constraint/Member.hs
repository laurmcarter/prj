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

module Data.Constraint.Member where

import Data.Constraint.Base
import Data.Constraint.Proof

import Data.Type.Equality
import Data.Type.Bool
import Data.Proxy

type (∈) = IsMember

-- | Expresses that 'a' is an element of the list 'as'.
class IsMember (a :: k) (as :: [k]) where
  member :: Member a as

instance IsMember a (a ': as) where
  member = Head

instance IsMember a as => IsMember a (b ': as) where
  member = Tail

data Member (a :: k) (as :: [k]) where
  Head ::             Member a (a ': as)
  Tail :: (a ∈ as) => Member a (b ': as)

deriving instance Show (Member a as)

emptyList :: Member a '[] -> b
emptyList _ = error "Invalid Member Proof"

instance Proof (Member a as) where
  type ProofOf (Member a as) = IsMember a as
  type Residue (Member a as) = EmptyC
  mem // r = case mem of
    Head -> r
    Tail -> r

