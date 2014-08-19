{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Monoid.Type where

import Data.Proxy
import GHC.Exts (Any)
import Data.Type.Equality
import Control.Applicative

class (tag ~ Any)
  => TMonoid (tag :: k) where
  type MemptyOf  tag                   :: k
  type MappendOf tag (x :: k) (y :: k) :: k
  ----
  tmempty :: Sing (MemptyOf tag)
  ----
  tmappend :: Sing x -> Sing y -> Sing (MappendOf tag x y)

class (tag ~ Any) => TMonoidVerified (tag :: k) where
  monoidLaws :: MonoidEvidence tag

type Mempty      = MemptyOf Any
type Mappend x y = MappendOf Any x y

type M0          = Mempty
type x <> y      = Mappend x y

data IdLeft  tag x     = IdLeft  { unIdLeft  :: !(MappendOf tag x  M0 :~: x) }
data IdRight tag x     = IdRight { unIdRight :: !(MappendOf tag M0 x  :~: x) }
data Assoc   tag x y z = Assoc   { unAssoc   :: !(MappendOf tag x (y <> z) :~: MappendOf tag (x <> y) z) }

data MonoidEvidence (tag :: k) = MonoidEvidence
  { idLeftLaw  :: forall x.     Sing x -> IdLeft  tag x
  , idRightLaw :: forall x.     Sing x -> IdRight tag x
  , assocLaw   :: forall x y z. Sing x -> Sing y -> Sing z -> Assoc tag x y z
  }

instance (tag ~ Any) => TMonoid (tag :: [k]) where
  type MemptyOf  tag            = '[]
  type MappendOf tag '[] y      = y
  type MappendOf tag (a ': x) y = a ': (x <> y)
  tmempty      = Nil
  tmappend xs ys = case xs of
    Nil      -> ys
    x :* xs' -> x :* tmappend xs' ys

instance (tag ~ Any) => TMonoidVerified (tag :: [k]) where
  monoidLaws = MonoidEvidence
    { idLeftLaw  =    IdLeft  . goIdL
    , idRightLaw =    IdRight . goIdR
    , assocLaw   = c3 Assoc     goAssoc
    }
    where
    goIdL :: forall as. List as -> (as <> M0) :~: as
    goIdL as = case as of
      Nil      -> Refl
      a :* as' -> apply Refl $ goIdL as'
    ----
    goIdR :: forall as. List as -> (M0 <> as) :~: as
    goIdR as = case as of
      Nil      -> Refl
      a :* as' -> apply Refl $ goIdR as'
    ----
    goAssoc :: forall (as :: [k]) bs cs. List as -> List bs -> List cs
      -> as <> (bs <> cs) :~: (as <> bs) <> cs
    goAssoc as bs cs = case as of
      Nil      -> Refl
      a :* as' -> apply Refl $ goAssoc as' bs cs
          

data family Sing (x :: k)

type List (as :: [k]) = Sing as

data instance Sing (as :: [k]) where
  Nil  :: List ('[] :: [k])
  (:*) :: Sing a -> List as -> List (a ': as)
infixr 4 :*

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a,b,c)

c3 :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
c3 f g = curry3 $ f . uncurry3 g

