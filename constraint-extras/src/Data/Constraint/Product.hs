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

module Data.Constraint.Product where

import Data.Constraint.Base
import Data.Constraint.Proof
import Data.Constraint.All
import Data.Proxy
import Data.Type.Bool

-- Product Constraint {{{

class    (a,b) => (a :: Constraint) :*: (b :: Constraint) where
  prodDict :: Prod Dict '[a,b]
instance (a,b) => (a :: Constraint) :*: (b :: Constraint) where
  prodDict = Dict :* Dict :* NilP

type instance Holds (a :*: b) = Holds a && Holds b

-- }}}

-- Prod Type {{{

data Prod (f :: k -> *) (as :: [k]) where
  NilP :: Prod f '[]
  (:*) :: f a -> Prod f as -> Prod f (a ': as)
infixr 4 :*

instance Proof (Prod f as) where
  type ProofOf (Prod f as) = ProofsOf f as
  type Residue (Prod f as) = (Proofs f as, Residues f as)
  as // r = case as of
    NilP     -> r
    a :* as' -> a // as' // r

{-
instance All (Show :.: f) as => Show (Prod f as) where
  showsPrec d as = case as of
    NilP     -> showString "NilP"
    (a :: f a) :* as' -> allInst (Proxy :: Proxy as)
      $ \(Dict :: Dict ((Show :.: f) a))
      -> showParen (d > 4)
       $ showsPrec 5 a
       . showString " :* "
       . showsPrec 4 as'
-}

-- }}}

