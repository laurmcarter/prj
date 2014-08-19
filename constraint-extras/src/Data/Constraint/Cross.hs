{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module Data.Constraint.Cross where

import Data.Constraint.Base
import Data.Constraint.Member
import Data.Constraint.All
import Data.Constraint.Each
import Data.Constraint.Proof

import Data.Proxy

-- | Expresses that every member of a list 'cs' of unary constraints,
--   each of kind (k -> Constraint), hold for every member
--   of a list 'as' of kind [k].
type Cross cs = All (Each cs)

crossInst :: forall cs as c a p0 p1 p2 p3.
  (Cross cs as, c ∈ cs, a ∈ as)
  => p0 cs -> p1 as -> p2 c -> p3 a -> Dict (c a)
crossInst (cs :: p0 cs) (as :: p1 as) (c :: p2 c) (a :: p3 a) = 
  eachInst cs c a \\ allInst as (Proxy :: Proxy (Each cs)) a

