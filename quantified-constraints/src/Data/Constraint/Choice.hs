{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Constraint.Choice where

import Data.Type.Bool
import Data.Type.Equality
import GHC.Exts (Constraint,Any)
import Data.Proxy

data List (f :: k -> *) (as :: [k]) where
  Nil  :: List f '[]
  (:>) :: f a -> List f as -> List f (a ': as)

data Proof (c :: Constraint) where
  Proof :: c => Proof c

deriving instance Show (Proof c)

type Proofs = List Proof

newtype Foo a = Foo { foo :: a }

newtype Bar a = Bar { bar :: Foo a }

type Outer (f :: * -> *) (g :: * -> *) = forall (a :: *). f a -> g a
type Inner (a :: *)      (b :: *)      = forall (f :: * -> *). f a -> f b
type Con   (f :: * -> *)               = forall (a :: *). a -> f a
type Dec   (f :: * -> *)               = forall (a :: *). f a -> a

{-
-- {{{
_Foo :: Iso x (Foo x)
_Foo = Con Foo foo

_Bar :: Iso (Foo x) (Bar x)
_Bar = Outer Bar bar

_FooBar :: Iso x (Bar x)
_FooBar = _Foo :& _Bar

_FFB :: Iso x (Bar (Foo x))
_FFB = _Foo :& _FooBar

data Iso (x :: *) (y :: *) where
  Id    :: Iso x x
  Con   :: Con f
        -> Dec f
        -> Iso x (f x)
  Dec   :: Dec f
        -> Con f
        -> Iso (f x) x
  Outer :: Outer f g
        -> Outer g f
        -> Iso (f x) (g x)
  Inner :: Inner a b
        -> Inner b a
        -> Iso (f a) (f b)
  (:&)  :: Iso a b -> Iso b c -> Iso a c
infixr 8 :&

instance Show (Iso a b) where
  showsPrec d i = case i of
    Id    {} -> showString "Id"
    Con   {} -> showString "Con"
    Dec   {} -> showString "Dec"
    Outer {} -> showString "Outer"
    Inner {} -> showString "Inner"
    f :& g   -> showParen (d > 8)
      $ showsPrec 9 f
      . showString " :& "
      . showsPrec 9 g

from :: Iso a b -> Iso b a
from i = case i of
  Id          -> Id
  Con   c  d  -> Dec d c
  Dec   d  c  -> Con c d
  Outer fg gf -> Outer gf fg
  Inner ab ba -> Inner ba ab
  f :& g      -> from g :& from f

applyIso :: Iso a b -> a -> b
applyIso i = case i of
  Id         -> id
  Con   c  _ -> c
  Dec   d  _ -> d
  Outer fg _ -> fg
  Inner ab _ -> ab
  f :& g     -> applyIso g . applyIso f

-- }}}
-}

data Iso (ts :: [*]) where
  Id    :: Iso '[]
  Con   :: Con f
        -> Dec f
        -> Iso ts
        -> Iso (ConT f ': ts)
  Dec   :: Dec f
        -> Con f
        -> Iso ts
        -> Iso (DecT f ': ts)
  Outer :: Outer (f :: * -> *) (g :: * -> *)
        -> Outer g f
        -> Iso ts
        -> Iso (OuterT f g ': ts)
  Inner :: Inner (a :: *) (b :: *)
        -> Inner b a
        -> Iso ts
        -> Iso (InnerT a b ': ts)
  Trans :: (a -> b)
        -> (b -> a)
        -> Iso ts
        -> Iso (TransT a b ': ts)

data OuterT :: (* -> *) -> (* -> *) -> *
data InnerT :: * -> * -> *
data TransT :: * -> * -> *
data ConT :: (* -> *) -> *
data DecT :: (* -> *) -> *

class (Dual (Dual t) ~ t) => HasDual (t :: *) where
  type Dual t :: *
  -- dual  :: Iso (t ': ts) -> Iso (Dual t ': ts)
instance HasDual (OuterT f g) where
  type Dual (OuterT f g) = OuterT g f
  -- dual (Outer fg gf i) = Outer gf fg i
instance HasDual (InnerT f g) where
  type Dual (InnerT f g) = InnerT g f
  -- dual (Inner ab ba i) = Inner ba ab i
instance HasDual (TransT a b) where
  type Dual (TransT a b) = TransT b a
  -- dual (Inner ab ba i) = Inner ba ab i
instance HasDual (ConT f) where
  type Dual (ConT f) = DecT f
  -- dual (Con c d i) = Dec d c i
instance HasDual (DecT f) where
  type Dual (DecT f) = ConT f
  -- dual (Dec d c i) = Con c d i

type family Duals (ts :: [*]) :: [*] where
  Duals '[]       = '[]
  Duals (t ': ts) = Dual t ': Duals ts

type family Apply (t :: *) (a :: *) :: * where
  Apply (ConT     f)    a  = f a
  Apply (DecT     f) (f a) =   a
  Apply (TransT a b)    a  =   b
  Apply (OuterT f g) (f a) = g a
  Apply (InnerT a b) (f a) = f b

class    Unrefined (x :: *)
instance Unrefined  x

{-
class    Refine (t :: *) (k :: * -> Constraint) (r :: Constraint) (x :: *) where
instance Refine (OuterT f g) k (x ~ f x_, k (g  x_)) x where
instance Refine (InnerT a b) k (x ~ x_ a, k (x_ b )) x where
instance Refine (ConT     f) k (          k (f  x )) x where
instance Refine (DecT     f) k (x ~ f x_, k     x_ ) x where
-}

class    (k (Apply t b))    => Refinement (t :: *) (k :: * -> Constraint) (b :: *) where
  refine :: prx k -> Refinements ts a b -> Refinements (t ': ts) a (Apply t b)

instance (x ~   a, k (f a)) => Refinement (ConT   f  ) k x where
  refine _ = Refined (Proof :: Proof (x ~ a, k (f a)))

instance (x ~ f a, k    a ) => Refinement (DecT   f  ) k x where
  refine _ = Refined (Proof :: Proof (x ~ f a, k a))

instance (x ~ f a, k (g a)) => Refinement (OuterT f g) k x where
  refine _ = Refined (Proof :: Proof (x ~ f a, k (g a)))

instance (x ~ f a, k (f b)) => Refinement (InnerT a b) k x where
  refine _ = Refined (Proof :: Proof (x ~ f a, k (f b)))

instance (x ~   a, k    b ) => Refinement (TransT a b) k x where
  refine _ = Refined (Proof :: Proof (x ~ a, k b))

type family Refine (ts :: [*]) :: * -> Constraint where
  Refine '[]       = Unrefined
  Refine (t ': ts) = Refinement t (Refine ts)

class Refines (ts :: [*]) (a :: *) (b :: *) where
  refinements :: Refinements ts a b

instance Refines '[] a a where
  refinements = Unrefined

instance Refinement t (Refines ts a) b => Refines (t ': ts) a b where
  refinements = refine (Proxy :: Proxy (Refines ts a))
    refinements

data Refinements (ts :: [*]) :: * -> * -> * where
  Unrefined :: Refinements '[] a a
  Refined   :: Proof c -> Refinements ts a b -> Refinements (t ': ts) a (Apply t b)

{-
r0 :: Refinements '[] a
r0 = refinements
r1 :: Refinements '[OuterT Foo Bar] (Foo a)
r1 = refinements
r2 :: Refinements '[OuterT Foo Bar,InnerT Int Bool] (Foo Int)
r2 = refinements
-- doesn't type check, refinements don't chain together
-- r3 :: Refinements '[OuterT Foo Bar,OuterT Foo Bar] (Foo a)
-- r3 = refinements
r3 :: Refinements '[OuterT Foo Bar,InnerT Int Bool,InnerT Bool Int] (Foo Int)
r3 = refinements
-}

{-
class    Refinement (ts :: [*]) (k :: * -> Constraint) (a :: *)
instance               k    a  => Refinement '[] k a
instance Refinement ts k (g a) => Refinement (OuterT f g ': ts) k (f a)
instance Refinement ts k (f b) => Refinement (InnerT a b ': ts) k (f a)
instance Refinement ts k (f a) => Refinement (ConT   f   ': ts) k    a
instance Refinement ts k    a  => Refinement (DecT   f   ': ts) k (f a)

type Refine (ts :: [*]) (a :: *) = Refinement ts Unrefined a
-}

from :: Iso ts -> Iso (Duals ts)
from i = case i of
  Id             -> Id
  Con   c  d  i' -> Dec   d  c  $ from i'
  Dec   d  c  i' -> Con   c  d  $ from i'
  Outer fg gf i' -> Outer gf fg $ from i'
  Inner ab ba i' -> Inner ba ab $ from i'
  Trans ab ba i' -> Trans ba ab $ from i'

{-
t0 :: Refines '[OuterT Foo Bar,InnerT Int Bool] x => x
t0 = case (refinements :: Refinements '[OuterT Foo Bar,InnerT Int Bool] (Foo Int)) of
  _ -> _
-}

{-
  ( Refine '[OuterT Foo Bar,InnerT Int Bool] x )
  ==>
  ( Refinement
    (OuterT Foo Bar)
    (Refine '[InnerT Int Bool])
    x
  )
  ==>
  ( x ~ Foo a
  , Refine '[InnerT Int Bool] (Bar a)
  )
  ==>
  ( x ~ Foo a
  , Refinement
    (InnerT Int Bool)
    (Refine '[])
    (Bar a)
  )
  ==>
  ( x ~ Foo a
  , Bar a ~ f Int
  , Refine '[] (f Bool)
  )
  ==>
  ( x ~ Foo a
  , Bar a ~ f Int
  , Unrefined (f Bool)
  )
  ==>
  ( x ~ Foo Int
  )
-}

{-
stepIso :: Proof (Refine ts a) => Iso ts -> a -> 
stepIso i a = case i of
  Con   c  _ i' -> (c a,i')
  Dec   d  _ i' -> _
  Outer fg _ i' -> undefined
  Inner ab _ i' -> undefined
-}

-- Member {{{

data Member (a :: k) (as :: [k]) where
  Head ::             Member a (a ': as)
  Tail :: (a ∈ as) => Member a (b ': as)

deriving instance Show (Member a as)

class (a :: k) ∈ (as :: [k]) where
  member :: Member a as

instance a ∈ (a ': as) where
  member = Head

instance (a ∈ as) => a ∈ (b ': as) where
  member = Tail

noMembers :: Member a '[] -> b
noMembers _ = error "noMembers: Empty membership proof"

intMem :: Member Int '[Bool,Double,Int]
intMem = member

-- }}}

-- All {{{

class All (c :: k -> Constraint) (as :: [k]) where
  withElem :: (b ∈ as)
    => proxy as
    -> (Proof (c b) -> r)
    -> r

instance All c '[] where
  withElem _ (f :: Proof (c b) -> r) = noMembers (member :: Member b '[])

instance (c a, All c as) => All c (a ': as) where
  withElem _ (f :: Proof (c b) -> r) = case (member :: Member b (a ': as)) of
    Head -> f Proof
    Tail -> withElem (Proxy :: Proxy as) f

-- }}}

