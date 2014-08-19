{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Data.Constraint.Refinement where

import Prelude hiding (id,(.))
import Control.Category (Category(..))
import qualified Control.Category as Cat
import Data.Type.Equality hiding (apply)
import qualified Data.Type.Equality as EQ
import GHC.Exts (Constraint)
import Data.Proxy

class Component (t :: *) where
  data Refine t :: * -> *
  type Apply t (a :: *) :: *
  applyComp :: t -> Refine t a -> a -> Apply t a

data Outer (f :: * -> *) (g :: * -> *) = Outer 
  (forall x. f x -> g x)
  (forall x. g x -> f x)

instance Component (Outer f g) where
  data Refine (Outer f g) a where
    OuterR :: Refine (Outer f g) (f a)
  type Apply (Outer f g) (f a) = g a
  applyComp (Outer fg _) OuterR = fg

deriving instance Show (Refine (Outer f g) a)

data F (f :: * -> *) = F (forall x. x -> f x) (forall x. f x -> x)

instance Component (F f) where
  data Refine (F f) a where
    FR :: Refine (F f) a
  type Apply (F f) a = f a
  applyComp (F c _) FR = c

{-
data Iso (ts :: [*]) where
  ID   :: Iso '[]
  (:&) :: Component t => t -> Iso ts -> Iso (t ': ts)
infixr 4 :&

iso :: Component t => t -> Iso '[t]
iso = (:& ID)

(&:) :: (Component t1, Component t2) => t1 -> t2 -> Iso '[t1,t2]
t1 &: t2 = t1 :& t2 :& ID
-}

class Refinement (t :: *) (a :: *) (b :: *) | t a -> b, t b -> a where
  type Dual t :: *
  dual  :: t -> Dual t
  apply :: t -> a -> b

instance Refinement (Outer f g) (f a) (g a) where
  type Dual (Outer f g) = Outer g f
  dual (Outer fg gf) = Outer gf fg
  apply (Outer fg _) = fg

data Iso (ts :: [*]) where
  ID   :: Iso '[]
  (:>) :: Composable1 t ts
       => t
       -> Iso ts
       -> Iso (t ': ts)
infixr 4 :>

class Composable1 (t :: *) (ts  :: [*]) where
instance
  ( Refinement t a b
  , Refinements ts b c
  ) => Composable1 t ts where

class Composable (ts1 :: [*]) (ts2 :: [*]) where
instance                                    Composable '[]         '[]         where
instance ( Composable1 t2         ts2  ) => Composable '[]         (t2 ': ts2) where
instance ( Composable1 t1         ts1
         , Composable  ts1 (t2 ': ts2) ) => Composable (t1 ': ts1) (t2 ': ts2) where

{-
instance
  ( Refinements ab a b
  , Refinements bc b c
  ) => Composable ab bc where
-}

{-
(<:) :: Composable ts '[t]  => Iso ts -> t -> Iso (ts |> t)
i <: t = case i of
  ID      -> t :> ID
  t' :> j -> undefined
-}

type a <| as = a ': as
infixr 5 <|
type as |> a = Snoc as a
infixl 5 |>

type family Snoc (as :: [k]) (a :: k) :: [k] where
  Snoc '[]       a = '[a]
  Snoc (b ': as) a = b ': Snoc as a

type family Reverse (as :: [k]) :: [k] where
  Reverse '[]       = '[]
  Reverse (a ': as) = Snoc (Reverse as) a

type family Duals (ts :: [*]) :: [*] where
  Duals '[]       = '[]
  Duals (t ': ts) = Dual t ': Duals ts

class Refinements (ts :: [*]) (a :: *) (b :: *) | ts a -> b, ts b -> a where
  to   :: Iso ts -> a -> b
  -- from :: Iso ts -> Iso (Duals ts)

instance Refinements '[] a a where
  to   ID a = a
  -- from ID   = ID

instance (Refinement t a b, Refinements ts b c) => Refinements (t ': ts) a c where
  to (t :> i)   = to i . apply t
  -- from (t :> i) = dual t :> from i

{-
data Iso (ts :: [*]) where
-}

newtype Foo a = Foo { foo :: a }
newtype Bar a = Bar { bar :: Foo a }

_Foo :: F Foo
_Foo = F Foo foo

_Bar :: Outer Foo Bar
_Bar = Outer Bar bar

(@:) :: (f :~: g) -> (a :~: b) -> f a :~: g b
(@:) = EQ.apply

