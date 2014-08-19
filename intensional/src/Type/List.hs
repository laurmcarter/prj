{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Type.List where

import Type.Context
import Type.Equality
import Type.Type
import Type.Witness
import Type.Value

import Type.Nat
import Theorems.Nat (addZ,addS)

data List (xs :: [k]) where
  NilL  :: List '[]
  ConsL :: Wit x x -> List xs -> List (x ': xs)

type IsList xs = WitBy xs List

-- Witness {{{

instance Witness '[] where
  type Wit '[] = List
  witness = NilL

instance (IsList xs, Witness x) => Witness (x ': xs) where
  type Wit (x ': xs) = List
  witness = ConsL witness (witness :: List xs)

-- }}}

-- Value {{{

instance Value a => Value1 '[] a where
  type Val1 '[] = []
  toVal1 NilL = []

{-
instance (IsList as, Value a, Value1 as a) => Value1 (a ': as) a where
  type Val1 (a ': as) = []
  toVal1 (ConsL a as) = toVal a : toVal1 as
-}

-- }}}

-- Append {{{

type family Append (xs :: [k]) (ys :: [k]) :: [k] where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

type xs :++ ys = Append xs ys
infixr 6 :++

append :: List xs -> List ys -> List (xs :++ ys)
append xs ys = case xs of
  NilL -> ys
  ConsL x xs' -> ConsL x $ append xs' ys

-- }}}

-- Length {{{

type family Length (xs :: [k]) :: N where
  Length '[] = Z
  Length (a ': xs) = S (Length xs)

len :: List xs -> Nat (Length xs)
len xs = case xs of
  NilL        -> NZ
  ConsL _ xs' -> NS $ len xs'

-- }}}

-- Map {{{

type Map (f :: x -> y) (xs :: [x]) = (FMap f xs :: [y])

{-
mapList :: Type f -> List xs -> List (Map f xs)
mapList f xs = case xs of
  NilL        -> NilL
  ConsL x xs' -> ConsL (f `tAp` x) (mapList f xs')
-}

-- }}}

