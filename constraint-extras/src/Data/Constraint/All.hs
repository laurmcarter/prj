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

module Data.Constraint.All where

import Data.Constraint.Base
import Data.Constraint.Member

import Data.Proxy

-- | A higher order constraint that expresses that a
--   unary constraint 'c' of kind (k -> Constraint) holds for
--   all members of the list 'as' of kind [k].
--   Thus, if some type 'a' is a member of 'as', then proof
--   that 'c a' holds can be freely produced.
class AllC c as => All (c :: k -> Constraint) (as :: [k]) where
  type AllC c as :: Constraint
  allInst :: IsMember a as => p0 as -> p1 c -> p2 a -> Dict (c a)

instance All c '[] where
  type AllC c '[] = EmptyC
  allInst _ _ (_ :: p2 a) = emptyList (member :: Member a '[])

instance (c a', All c as) => All c (a' ': as) where
  type AllC c (a' ': as) = (c a', All c as)
  allInst _ (c :: p1 c) (a :: p2 a) =
    case (member :: Member a (a' ': as)) of
      Head -> Dict
      Tail -> allInst (Proxy :: Proxy as) c a

