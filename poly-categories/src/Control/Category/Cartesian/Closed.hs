{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Category.Cartesian.Closed where

import qualified Prelude
import Prelude hiding (id,(.),fst,snd,curry,uncurry)

import Data.Semigroupoid
import Control.Categorical
import Control.Categorical.Bifunctor
import Control.Category.Dual
import Control.Category.Braided
import Control.Category.Cartesian

class Cartesian k => CCC (k :: x -> x -> *) where
  type Exp k :: x -> x -> x
  apply   :: Product k (Exp k a b) a `k` b
  curry   :: (Product k a b `k` c) -> a `k` Exp k b c
  uncurry :: (a `k` Exp k b c) -> Product k a b `k` c

instance CCC (->) where
  type Exp (->) = (->)
  apply (f,x) = f x
  curry       = Prelude.curry
  uncurry     = Prelude.uncurry

instance CoCCC k => CCC (Dual k) where
  type Exp (Dual k) = Coexp k
  apply   = Dual coapply
  curry   = Dual . cocurry   . runDual
  uncurry = Dual . uncocurry . runDual

unitCCC :: CCC k => a `k` Exp k b (Product k b a)
unitCCC = curry braid

counitCCC :: CCC k => Product k b (Exp k b a) `k` a
counitCCC = apply . braid

class CoCartesian k => CoCCC (k :: x -> x -> *) where
  type Coexp k :: x -> x -> x
  coapply   :: b `k` Sum k (Coexp k a b) a
  cocurry   :: c `k` Sum k a b -> Coexp k b c `k` a
  uncocurry :: Coexp k b c `k` a -> c `k` Sum k a b

instance CCC k => CoCCC (Dual k) where
  type Coexp (Dual k) = Exp k
  coapply   = Dual apply
  cocurry   = Dual . curry . runDual
  uncocurry = Dual . uncurry . runDual

unitCoCCC :: CoCCC k => a `k` Sum k b (Coexp k b a)
unitCoCCC = swap . coapply

counitCoCCC :: CoCCC k => Coexp k b (Sum k b a) `k` a
counitCoCCC = cocurry swap

