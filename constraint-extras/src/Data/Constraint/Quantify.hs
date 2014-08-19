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

module Data.Constraint.Quantify where

import Data.Constraint.Base
import Data.Constraint.Dict
import Data.Constraint.Proof

import Data.Type.Equality
import Data.Proxy

class A (c :: k -> Constraint) where
  universal :: Uni (Dict1 c)

class E (c :: k -> Constraint) where
  existential :: Exi (Dict1 c)

class E1 (c :: k -> Constraint) where
  existUnique :: Exi1 (Dict1 c)

data Uni (p :: k -> *) = Uni
  { uni :: forall (a :: k). p a
  }

data Exi  (p :: k -> *) = forall (a :: k). Exi (p a)

data Exi1 (p :: k -> *) = forall (a :: k).
  Exi1 (p a) (forall (b :: k). p b -> a :~: b)

class Foo (a :: Bool) (b :: *) (c :: *)

instantiateUni :: forall c a.
  Uni (Dict1 (A :.: c)) -> Dict (A (c a))
instantiateUni up = Dict \\. (uni up :: Dict1 (A :.: c) a)

instantiateA :: Dict (A c) -> Dict1 c a
instantiateA = (uni universal \\)

distUni :: forall (c :: k -> l -> Constraint).
  Uni (Dict1 (A :.: c)) -> Uni (Uni .: Dict2 c)
distUni d0 = Uni $ Comp $ Uni d3
  where
  d1 :: forall (a :: k). Dict (A (c a))
  d1 = instantiateUni d0
  d2 :: forall (a :: k) (b :: l). Dict1 (c a) b
  d2 = instantiateA d1
  d3 :: forall (a :: k) (b :: l). Dict2 c a b
  d3 = Dict2 \\ (d2 :: Dict1 (c a) b)

