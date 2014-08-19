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

module Type.Context where

import Type.Bool
import Type.Equality
import Type.Type
import Type.Witness

type a :=: b  = a ':*: b
infixr 6 :=:

-- Freezing {{{

data Context (ps :: [Product k l]) where
  NilC  :: Context '[]
  (:&) :: Eqv a b -> Context ps -> Context ((a :=: b) ': ps)
infixr 5 :&

-- }}}

-- Unfreezing {{{

type family Presuming (ps :: [Product k l]) :: Constraint where
  Presuming '[] = ()
  Presuming (a :=: b ': ps) = (a ~ b, Presuming ps)

-- }}}

-- Building {{{

class Premises s (ps :: [Product k l]) | s -> ps where
  (&)     :: Eqv a b -> s -> Context (a :=: b ': ps)
  (|-)    :: s -> (Presuming ps => r) -> r

infixr 5 &
infixr 4 |-

instance Premises (Eqv c d) '[c :=: d] where
  p & q = p :& q :& NilC
  Refl |- r = r

instance Premises (Context ps) ps where
  p & env = p :& env
  env |- r = case env of
    NilC -> r
    Refl :& env' -> env' |- r

-- }}}

-- Type Case {{{

type family Unifies (a :: k) (b :: l) :: Bool where
  Unifies a a = True
  Unifies a b = False

typeCase :: IsBool (Unifies a b) => Type (a :=: b) -> Boolean (Unifies a b)
typeCase Type = witness

-- }}}

imply :: Presuming ps => Context ps -> Eqv a a
imply ps = ps |- refl

class Implies ps qs where
  implies :: Presuming ps => Context ps -> Context qs

instance Implies ps '[] where
  implies _ = NilC

instance Implies ps qs => Implies ps (a :=: a ': qs) where
  implies ps = imply ps :& implies ps

