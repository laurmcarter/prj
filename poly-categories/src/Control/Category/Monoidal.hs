{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Category.Monoidal where

import qualified Prelude
import Prelude hiding (id,(.))

import Data.Semigroupoid
import Control.Categorical
import Control.Category.Dual
import Control.Category.Associative
import Data.Void

class Associative k p => Monoidal (k :: x -> x -> *) (p :: x -> x -> x) where
  type Id k p :: x
  idl   :: p (Id k p) a `k` a
  idr   :: p a (Id k p) `k` a
  coidl :: a `k` p (Id k p) a
  coidr :: a `k` p a (Id k p)

instance Monoidal (->) Either where
  type Id (->) Either = Void
  idl = either absurd id
  idr = either id absurd
  coidl = Right
  coidr = Left

instance Monoidal (->) (,) where
  type Id (->) (,) = ()
  idl = snd
  idr = fst
  coidl a = ((),a)
  coidr a = (a,())

instance Monoidal k p => Monoidal (Dual k) p where
  type Id (Dual k) p = Id k p
  idl   = Dual coidl
  idr   = Dual coidr
  coidl = Dual idl
  coidr = Dual idr

