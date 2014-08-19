{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

module Data.Type.Defunction where

data F (a :: *) (b :: *) = Fun a b

type family (f :: F k l -> *) $ (x :: k) :: l where
  (f :: F k (F l m -> *) -> *) $ (x :: k) = f :@ x
  (f :: F k  l           -> *) $ (x :: k) = f @: x
infixl 9 $

data (f :: F k (F l m -> *) -> *) :@ (x :: k) :: F l m -> *
infixl 9 :@

type family (f :: F k l -> *) @: (x :: k) :: l
infixl 8 @:

data Id :: F k k -> *
type instance Id @: x = x

data Const :: F k (F l k -> *) -> *
type instance (Const :@ x) @: y = x

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
infixr 8 .:
{-# INLINE (.:) #-}

onT :: (g a -> g b -> c) -> (forall a. f a -> g a) -> f a -> f b -> c
onT op f a b = f a `op` f b
{-# INLINE onT #-}

underT :: (forall a. f a -> g a) -> (forall a. g a -> f a) -> (g a -> g b -> g c) -> f a -> f b -> f c
underT to_ from_ op = from_ .: op `onT` to_
{-# INLINE underT #-}

