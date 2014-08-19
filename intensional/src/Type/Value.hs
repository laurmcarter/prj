{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types #-}

module Type.Value where

import Type.Witness

class Witness t => Value (t :: k) where
  type Val t :: *
  toVal :: Wit t t -> Val t

type Reps t v = (Value t, Val t ~ v)

class (Witness t, Value a) => Value1 t a where
  type Val1 t :: * -> *
  toVal1 :: Wit t t -> Val1 t (Val a)

class AllVal (v :: *) (as :: [k])
instance AllVal v '[]
instance (AllVal v as, Reps a v) => AllVal v (a ': as)

