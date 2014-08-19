{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}

module Control.Apply where

import Control.Applicative (Const(..))
import Data.Semigroup
import Data.Monoid (Monoid(..))
import GHC.Exts (Constraint)

class Apply (r :: * -> *) where
  (<.>) :: r (a -> b) -> r a -> r b
  infixl 4 <.>

class Apply r => Pure (r :: * -> *) where
  pure :: a -> r a

class Apply r => HOAS (r :: * -> *) where
  abs :: (r a -> r b) -> r (a -> b)

class Pure r => PHOAS (r :: * -> *) where
  wabs :: (a -> r b) -> r (a -> b)

instance Semigroup r => Apply (Const r) where
  Const m <.> Const n = Const $ m <> n

instance Monoid r => Pure (Const (WrappedMonoid r)) where
  pure _ = cempty

cempty :: Monoid r => Const (WrappedMonoid r) a
cempty = Const $ WrapMonoid mempty

instance Monoid r => HOAS (Const (WrappedMonoid r)) where
  abs f = retype $ f cempty

instance Monoid r => PHOAS (Const (WrappedMonoid r)) where
  wabs f = cempty

class Inhabit (t :: * -> *) where
  type Require t (a :: *) (b :: *) :: Constraint
  retype :: Require t a b => t a -> t b

instance Inhabit (Const r) where
  type Require (Const r) a b = (() :: Constraint)
  retype (Const r) = Const r

instance Show r => Show (Const r a) where
  showsPrec d (Const r) = showParen (d > 10)
    $ showString "Const "
    . showsPrec 11 r

cwrap :: r -> Const (WrappedMonoid r) a
cwrap = Const . WrapMonoid

t0 :: PHOAS r => r (a -> a)
t0 = wabs pure

t1 :: PHOAS r => r Int
t1 = t0 <.> pure 3

