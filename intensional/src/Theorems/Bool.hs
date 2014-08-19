{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Theorems.Bool where

import Type.Bool

import Theorems.Base

deMorganConj :: Boolean x -> Boolean y
  -> Not (x :&& y) :== Not x :|| Not y
deMorganConj x y = case (x,y) of
  (BF,_ ) -> refl
  (_ ,BF) -> refl
  (BT,BT) -> refl

deMorganDisj :: Boolean x -> Boolean y
  -> Not (x :|| y) :== Not x :&& Not y
deMorganDisj x y = case (x,y) of
  (BF,BF) -> refl
  (_ ,BT) -> refl
  (BT,_ ) -> refl

