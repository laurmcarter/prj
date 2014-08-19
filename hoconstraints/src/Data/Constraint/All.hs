{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Constraint.All where

import Data.Constraint.Member
import Data.Constraint.Dict

import GHC.Prim (Constraint)
import Data.Proxy

class All (c :: k -> Constraint) (as :: [k]) where
  withElem :: Member as b => Proxy as -> (Dict1 c b -> r) -> r

instance All c '[] where
  withElem _ (_ :: Dict1 c b -> r) = seq (evidence :: Evidence '[] b) undefined

instance (c a, All c as) => All c (a ': as) where
  withElem _ (f :: Dict1 c b -> r) =
    case (evidence :: Evidence (a ': as) b) of
      Head -> f Dict1
      Tail -> withElem (Proxy :: Proxy as) f

class AllT (tag :: k -> l) (c :: k -> Constraint) (as :: [l]) where
  withTagged :: Member as (tag b) => Proxy tag -> Proxy as -> (Dict1 c b -> r) -> r

instance AllT tag c '[] where
  withTagged _ _ (_ :: Dict1 c b -> r) =
    seq (evidence :: Evidence '[] (tag b)) undefined

instance (c a, AllT tag c as) => AllT tag c (tag a ': as) where
  withTagged ptag _ (f ::Dict1 c b -> r) =
    case (evidence :: Evidence (tag a ': as) (tag b)) of
      Head -> f Dict1
      Tail -> withTagged ptag (Proxy :: Proxy as) f

class Each (cs :: [k -> Constraint]) (t :: k) where
  eachDict :: DictList1 cs t

instance Each '[] t where
  eachDict = NilD1

instance (c t, Each cs t) => Each (c ': cs) t where
  eachDict = ConsD1 Dict1 eachDict

class EachT (tag :: (k -> Constraint) -> l) (cs :: [l]) (t :: k) where

instance EachT tag '[] t

instance (c t, EachT tag cs t) => EachT tag (tag c ': cs) t

instance EachT tag cs t => EachT tag (xxx (c :: k -> Constraint) ': cs) (t :: k)

type AllIn bs as = All (Member bs) as

