{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Constraint.Dict where

import GHC.Prim (Constraint)

data Dict c where
  Dict :: c => Dict c

instance Show (Dict c) where
  show Dict = "Dict"

data Dict1 c a where
  Dict1 :: c a => Dict1 c a

data Dict2 c a b where
  Dict2 :: c a b => Dict2 c a b

data DictList (cs :: [Constraint]) where
  NilD  :: DictList '[]
  ConsD :: Dict c -> DictList cs -> DictList (c ': cs)

data DictList1 (cs :: [k -> Constraint]) (a :: k) where
  NilD1  :: DictList1 '[] a
  ConsD1 :: Dict1 c a -> DictList1 cs a -> DictList1 (c ': cs) a

data DictList2 (cs :: [k -> l -> Constraint]) (a :: k) (b :: l) where
  NilD2  :: DictList2 '[] a b
  ConsD2 :: Dict2 c a b -> DictList2 cs a b -> DictList2 (c ': cs) a b

