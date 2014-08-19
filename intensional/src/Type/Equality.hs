{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Type.Equality
  ( module Type.Equality
  , Constraint
  ) where

import Type.Type
import GHC.Exts (Constraint)

data Eqv a b where
  Refl :: Eqv a a

infixr 4 :==

type (:==) = Eqv

eqv :: a :== b -> Type a -> Type b
eqv Refl a = a
{-# INLINE eqv #-}

sym :: a :== b -> b :== a
sym Refl = refl
{-# INLINE sym #-}

refl :: a :== a
refl = Refl
{-# INLINE refl #-}

trans :: b :== c -> a :== b -> a :== c
trans Refl Refl = refl
{-# INLINE trans #-}

fEqv :: Type f -> a :== b -> f a :== f b
fEqv Type Refl = refl
{-# INLINE fEqv #-}

fEqv2 :: Type p -> a :== b -> c :== d -> p a c :== p b d
fEqv2 Type Refl Refl = refl
{-# INLINE fEqv2 #-}

{-
class Quantifiable p a b | p -> a b where
  quantify :: p -> a :== b
instance Quantifiable (a :== b) a b where
  quantify = id
instance (Quantifiable p a b, WitBy t w)
  => Quantifiable (w t -> p) a b where
  quantify (f :: w t -> p) = quantify $ f (witness :: w t)
-}

