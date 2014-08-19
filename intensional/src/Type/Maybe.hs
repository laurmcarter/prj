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

module Type.Maybe where

import Type.Equality
import Type.Type
import Type.Nat
import Type.Witness
import Type.Value

data MaybeP (a :: Maybe k) where
  NothingP :: MaybeP Nothing
  JustP    :: Type a -> MaybeP (Just a)

-- Witness {{{

type IsMaybe m = WitBy m MaybeP

instance Witness Nothing where
  type Wit Nothing = MaybeP
  witness = NothingP

instance Witness (Just a) where
  type Wit (Just a) = MaybeP
  witness = JustP Type

-- }}}

{-
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
  ConsL Type xs' -> NS $ len xs'

-- }}}

-- Map {{{

type Map (f :: x -> y) (xs :: [x]) = (FMap f xs :: [y])

mapList :: Type f -> List xs -> List (Map f xs)
mapList f xs = case xs of
  NilL        -> NilL
  ConsL x xs' -> ConsL (f `tAp` x) (mapList f xs')

-- }}}
-}

