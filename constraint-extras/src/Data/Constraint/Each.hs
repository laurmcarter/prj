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

module Data.Constraint.Each where

import Data.Constraint.Base
import Data.Constraint.Member
import Data.Constraint.Proof

import Data.Proxy

-- | A higher order constraint that expresses that a
--   list 'cs' of unary constraints of kind (k -> Constraint) each
--   hold for a type 'a' of kind k.
--   Thus, if some type 'c' is a member of 'cs', then proof
--   that 'c a' holds can be freely produced.
class EachC cs a => Each (cs :: [k -> Constraint]) (a :: k) where
  type EachC cs a :: Constraint
  eachInst :: IsMember c cs => p0 cs -> p1 c -> p2 a -> Dict (c a)

instance Each '[] a where
  type EachC '[] a = EmptyC
  eachInst _ (_ :: p1 c) _ = emptyList (member :: Member c '[])

instance (c a, Each cs a) => Each (c ': cs) a where
  type EachC (c ': cs) a = (c a, Each cs a)
  eachInst _ (d :: p1 d) (a :: p2 a) =
    case (member :: Member d (c ': cs)) of
      Head -> Dict
      Tail -> eachInst (Proxy :: Proxy cs) d a

