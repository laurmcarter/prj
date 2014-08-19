{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}

module Type.Relation.Binary.Classes where

import Data.Bifunctor
import Data.Functor.Contravariant (Contravariant(..))
import Data.Type.Equality ((:~:)(..))
import qualified Data.Type.Equality as Eq
import Data.Void
import GHC.Exts (Any,Constraint)

data Exists (f :: k -> *) = forall (a :: k). Exists
  { exists :: f a
  }

data Forall (f :: k -> *) = Forall
  { forall :: forall (a :: k). f a
  }

newtype Op (r :: k -> k -> *) (a :: k) (b :: k) = Op
  { op :: r b a
  } deriving (Eq,Show)

type Not a = a -> Void

newtype Neg r a b = Neg
  { neg :: Not (r a b)
  }

-- Relation / Rel {{{

-- | The type family Relation provides a way to
-- embed arbitrary binary relations as values.
-- The newtype Rel returns injectivity to the use of
-- the type family.
type family   Relation (r :: k -> k -> l) :: k -> k -> *
type instance Relation (Rel r) = Relation r

-- | to think on: what are the differences between
-- a type family / newtype pair, and a data family?

newtype Rel (r :: k -> k -> l) (a :: k) (b :: k) = Rel
  { rel :: Relation r a b
  }

deriving instance Eq   (Relation r a b) => Eq   (Rel r a b)
deriving instance Show (Relation r a b) => Show (Rel r a b)

-- }}}

-- Reflexive {{{

class Reflexive (r :: k -> k -> *) where
  refl :: r a a

instance Reflexive (->) where
  refl = id

instance Reflexive (:~:) where
  refl = Refl

instance Reflexive r => Reflexive (Op r) where
  refl = Op refl

instance Irreflexive r => Reflexive (Neg r) where
  refl = Neg irrefl

instance ReflexiveC r => Reflexive (Rel r) where
  refl = reflC

class ReflexiveC (r :: k -> k -> Constraint) where
  reflC :: Rel r a a

-- }}}

-- Symmetric {{{

class Symmetric (r :: k -> k -> *) where
  symm :: r a b -> r b a

instance Symmetric (:~:) where
  symm Refl = Refl

instance Symmetric r => Symmetric (Op r) where
  symm = Op . symm . op

instance Symmetric r => Symmetric (Neg r) where
  symm r = Neg $ neg r . symm

class SymmetricC (r :: k -> k -> Constraint) where
  symmC :: Rel r a b -> Rel r b a

instance SymmetricC r => Symmetric (Rel r) where
  symm = symmC

-- }}}

-- Transitive {{{

class Transitive (r :: k -> k -> *) where
  trans :: r a b -> r b c -> r a c

instance Transitive (->) where
  trans = flip (.)

instance Transitive (:~:) where
  trans Refl Refl = Refl

instance Transitive r => Transitive (Op r) where
  trans f g = Op $ op g `trans` op f

instance TransitiveC r => Transitive (Rel r) where
  trans = transC

class TransitiveC (r :: k -> k -> Constraint) where
  transC :: Rel r a b -> Rel r b c -> Rel r a c

-- }}}

-- Equivalence {{{

type Equivalence (r :: k -> k -> *) =
  ( PartialEquivalence r
  , Reflexive  r
  )

type PartialEquivalence (r :: k -> k -> *) =
  ( Symmetric  r
  , Transitive r
  )

type EquivalenceC (r :: k -> k -> Constraint) =
  ( PartialEquivalenceC r
  , ReflexiveC  r
  )

type PartialEquivalenceC (r :: k -> k -> Constraint) =
  ( SymmetricC  r
  , TransitiveC r
  )

-- }}}

-- Equality {{{

class Equivalence r => Equality (r :: k -> k -> *) where
  fromEquality :: a :~: b -> r a b
  toEquality   :: r a b   -> a :~: b
  asEquality   :: Equality eq => r a b -> eq a b
  asEquality   = fromEquality . toEquality

instance Equality (:~:) where
  fromEquality = id
  toEquality   = id
  asEquality   = fromEquality

-- }}}

-- Substitution

-- Subst {{{

class Subst (r :: k -> k -> *) (f :: k -> *) where
  subst :: r a b -> f a -> f b

instance Functor f => Subst (->) f where
  subst = fmap

instance Subst (:~:) f where
  subst Refl = id

instance (q ~ Relation r, Subst q f) => Subst (Rel r) f where
  subst = subst . rel

instance Unsubst r f => Subst (Op r) f where
  subst = unsubst . op

-- }}}

-- Unsubst {{{

class Unsubst (r :: k -> k -> *) (f :: k -> *) where
  unsubst :: r a b -> f b -> f a

instance Contravariant f => Unsubst (->) f where
  unsubst = contramap

instance Unsubst (:~:) f where
  unsubst Refl = id

instance (q ~ Relation r, Unsubst q f) => Unsubst (Rel r) f where
  unsubst = unsubst . rel

instance Subst r f => Unsubst (Op r) f where
  unsubst = subst . op

-- }}}



-- Inequality {{{

type Inequality (r :: k -> k -> *) =
  ( Irreflexive r
  , Symmetric   r
  )

-- }}}

-- Connected {{{

class Equivalence eq => Connected (r :: k -> k -> *) eq where
  conn :: Not (r a b) -> Not (r b a) -> eq a b

instance Connected r eq => Connected (Op r) eq where
  conn nr ns = symm $ conn (nr . Op) (ns . Op)

instance ConnectedC r eq => Connected (Rel r) eq where
  conn = connC

class Equivalence eq => ConnectedC (r :: k -> k -> Constraint) eq where
  connC :: Not (Rel r a b) -> Not (Rel r b a) -> eq a b

-- }}}

-- Comparison {{{

class Comparison (r :: k -> k -> *) where
  comparison :: w b -> r a c -> Either (r a b) (r b c)

instance Comparison r => Comparison (Op r) where
  comparison b = bimap Op Op . coswap . comparison b . op

class ComparisonC (r :: k -> k -> Constraint) where
  comparisonC :: w b -> Rel r a c -> Either (Rel r a b) (Rel r b c)

-- }}}

-- Total {{{

class Total (r :: k -> k -> *) where
  total :: w a -> w b -> Either (r a b) (r b a)

instance Total r => Total (Op r) where
  total a b = bimap Op Op $ coswap $ total a b

instance TotalC r => Total (Rel r) where
  total = totalC

class TotalC (r :: k -> k -> Constraint) where
  totalC :: w a -> w b -> Either (Rel r a b) (Rel r b a)

-- }}}

-- Reflexive variants

-- Irreflexive {{{

class Irreflexive (r :: k -> k -> *) where
  irrefl :: Not (r a a)

instance Irreflexive r => Irreflexive (Op r) where
  irrefl = irrefl . op

instance IrreflexiveC r => Irreflexive (Rel r) where
  irrefl = irreflC

class IrreflexiveC (r :: k -> k -> Constraint) where
  irreflC :: Not (Rel r a a)

-- }}}

-- Coreflexive {{{

class Equivalence eq => Coreflexive (r :: k -> k -> *) eq where
  corefl :: r a b -> eq a b

-- }}}

-- Symmetric variants

-- Antisymmetric {{{

class Equivalence eq => Antisymmetric (r :: k -> k -> *) eq where
  antisymm :: r a b -> r b a -> eq a b

instance Antisymmetric r eq => Antisymmetric (Op r) eq where
  antisymm r s = symm $ antisymm (op r) (op s)

-- }}}

-- Asymmetric {{{

class Equivalence eq => Asymmetric (r :: k -> k -> *) eq where
  asymm :: r a b -> Not (eq a b)

instance Asymmetric r eq => Asymmetric (Op r) eq where
  asymm r = asymm (op r) . symm

-- }}}

-- Function

-- Functional {{{

class Equivalence eq => Functional (r :: k -> k -> *) eq where
  span :: r a b -> r a c -> eq b c

class PartialEquivalence eq => PartialFunctional (r :: k -> k -> *) eq where
  spanPart :: r a b -> r a c -> eq b c

-- }}}

-- Entire {{{

class Entire (r :: k -> k -> *) where
  type Image r (a :: k) :: k
  entire :: w a -> r a (Image r a)

-- }}}

-- Function {{{

type Function (r :: k -> k -> *) eq =
  ( Functional r eq
  , Entire r
  )

-- }}}

-- Util

coswap :: Either a b -> Either b a
coswap = \case
  Left  a -> Right a
  Right b -> Left  b

