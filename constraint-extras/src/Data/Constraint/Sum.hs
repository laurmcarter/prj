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

module Data.Constraint.Sum where

import Data.Constraint.Base
import Data.Constraint.Member
import Data.Constraint.Proof

import Data.Type.Bool
import Data.Proxy

-- Sum Constraint {{{

class (a :: Constraint) :+: (b :: Constraint) where
  sumDict :: Sum Dict
    (If (Holds a)
        (a ': If (Holds b) '[b] '[])
        (     If (Holds b) '[b] '[]))
    '[a,b]

instance (pa ~ Holds a, pb ~ Holds b, SumConstraint pa pb a b) => a :+: b where
  sumDict = sumDict' (Proxy :: Proxy (Holds a)) (Proxy :: Proxy (Holds b))

class ((pa || pb) ~ True) => SumConstraint (pa :: Bool) (pb :: Bool) (a :: Constraint) (b :: Constraint) where
  sumDict' :: p0 pa -> p1 pb -> Sum Dict
    (If pa
        (a ': If pb '[b] '[])
        (     If pb '[b] '[]))
    '[a,b]

instance a => SumConstraint True False a b where
  sumDict' _ _ = Yes Dict :+ No :+ NilS

instance b => SumConstraint False True a b where
  sumDict' _ _ = No :+ Yes Dict :+ NilS

instance (a,b) => SumConstraint True True a b where
  sumDict' _ _ = Yes Dict :+ Yes Dict :+ NilS

type instance Holds (a :+: b) = Holds a || Holds b

-- }}}

-- Sum Type {{{

data Sum (f :: k -> *) (as :: [k]) (bs :: [k]) where
  NilS :: Sum f '[] as
  (:+) :: Option p (f a)
        -> Sum f as bs
        -> Sum f (If p (a ': as) as) (a ': bs)
infixr 4 :+

instance Show (Sum f as '[]) where
  show NilS = "NilS"

data CSum (c :: [k] -> Constraint) (f :: k -> *) (as :: [k]) where 
  CSum :: c as => Sum f as bs -> CSum c f bs

(.+) :: f a -> Sum f as bs -> Sum f (a ': as) (a ': bs)
a .+ as = Yes a :+ as
infixr 6 .+

blank :: Sum f as bs -> Sum f as (a ': bs)
blank = (No :+)

-- }}}

-- Injection Projection {{{

inj :: forall f a as bs. (a ∈ bs) => f a -> Sum f '[a] bs
inj = case (member :: Member a bs) of
  Head -> (.+ NilS)
  Tail -> blank . inj

prj :: forall f a as bs. (a ∈ as) => Sum f as bs -> f a
prj as = case as of
  NilS             -> emptyList (member :: Member a '[])
  No    :+ as' -> prj as'
  Yes a :+ as' -> case (member :: Member a as) of
    Head -> a
    Tail -> prj as'

mPrj :: forall f a as bs. (a ∈ bs) => Sum f as bs -> Maybe (f a)
mPrj as = case as of
  NilS             -> Nothing
  No    :+ as' -> case (member :: Member a bs) of
    Head -> Nothing
    Tail -> mPrj as'
  Yes a :+ as' -> case (member :: Member a bs) of
    Head -> Just a
    Tail -> mPrj as'

-- }}}

-- Option {{{

data Option (p :: Bool) (a :: *) where
  Yes :: a -> Option True a
  No  :: Option False a

optProof :: (Proof t, Residue t) => Option p t -> ((p ~ True, ProofOf t) => r) -> Option p r
optProof mt r = case mt of
  Yes t -> Yes $ t // r
  No    -> No

instance Functor (Option p) where
  fmap f opt = case opt of
    Yes a -> Yes $ f a
    No    -> No

deriving instance Show a => Show (Option p a)

-- }}}

