{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module CLP where

import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality
import Data.Proxy

data Id (x :: Symbol) where
  Id :: KnownSymbol x => Id x

instance TestEquality Id where
  testEquality (Id :: Id x) (Id :: Id y) =
    sameSymbol (Proxy :: Proxy x) (Proxy :: Proxy y)

data List (f :: k -> *) (as :: [k]) where
  Nil  :: List f '[]
  (:>) :: f a -> List f as -> List f (a ': as)
infixr 4 :>

data Uncurry (f :: k -> l -> *) :: (k,l) -> * where
  Uncurry :: f a b -> Uncurry f '(a,b)

data Field (f :: k -> *) (x :: Symbol) (a :: k) where
  Field :: Id x -> f a -> Field f x a

type Dict f = List (Uncurry (Field f))

(=:) :: Id x -> f a -> Dict f '[ '(x,a) ]
x =: a = Uncurry (Field x a) :> Nil
infix 1 =:

type family Union (as :: [(Symbol,k)]) (bs :: [(Symbol,k)]) :: [(Symbol,k)]

type family Remove (x :: Symbol) (as :: [(Symbol,k)]) :: [(Symbol,k)] where
  Remove x  '[]           = '[]
  Remove x ('(x,a) ': as) = Remove x as
  Remove x ('(y,a) ': as) = '(y,a) ': Remove x as

data B (x :: Bool) where
  True_  :: B True
  False_ :: B False

