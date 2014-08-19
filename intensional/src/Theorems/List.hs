{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Theorems.List where

import Type.List
import Type.Nat

import Theorems.Base
import Theorems.Nat (addZ,addS)

appendNil :: List xs -> xs :++ '[] :== xs
appendNil xs = case xs of
  NilL        -> refl
  ConsL _ xs' -> appendNil xs' |- refl

appendLenComm :: List xs -> List ys
  -> Length (xs :++ ys) :== Length (ys :++ xs)
appendLenComm xs ys = case (xs,ys) of
  (NilL,_) -> appendNil ys |- refl
  (_,NilL) -> appendNil xs |- refl
  (ConsL _ xs',ConsL _ ys') ->
      appendLenComm xs' ys
    & appendLenComm xs  ys'
    & appendLenComm xs' ys'
    |- refl

appendLenDist :: List xs -> List ys ->
  Length (xs :++ ys) :== Length xs :+ Length ys
appendLenDist xs ys = case xs of
  NilL        -> refl
  ConsL _ xs' -> case ys of
    NilL ->
         addZ (len xs')
      |- appendNil xs'
      |- refl
    ConsL _ ys' ->
         addS (len xs') (len ys')
      & appendLenDist xs' ys
      & appendLenDist xs ys'
      & appendLenDist xs' ys'
      |- refl

mapLength :: Type f -> List xs
  -> Length xs :== Length (Map f xs)
mapLength f xs = case xs of
  NilL        -> refl
  ConsL _ xs' -> mapLength f xs' |- refl

