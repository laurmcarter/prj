{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Category.Cartesian where

import qualified Prelude
import Prelude hiding (id,(.),fst,snd)

import Data.Semigroupoid
import Control.Categorical
import Control.Categorical.Bifunctor
import Control.Category.Dual
import Control.Category.Braided
import Control.Category.Monoidal

class (Monoidal k (Product k), Symmetric k (Product k))
  => Cartesian (k :: x -> x -> *) where
  type Product k :: x -> x -> x
  fst   :: Product k a b `k` a
  snd   :: Product k a b `k` b
  diag  :: a `k` Product k a a
  (&&&) :: k a b -> k a c -> a `k` Product k b c
  infixr 3 &&&

instance Cartesian (->) where
  type Product (->) = (,)
  fst         = Prelude.fst
  snd         = Prelude.snd
  diag a      = (a,a)
  (f &&& g) a = (f a, g a)

instance CoCartesian k => Cartesian (Dual k) where
  type Product (Dual k) = Sum k
  fst     = Dual inl
  snd     = Dual inr
  diag    = Dual codiag
  f &&& g = Dual $ runDual f ||| runDual g

bimapProduct :: Cartesian k
  => k a c -> k b d
  -> (Product k a b `k` Product k c d)
bimapProduct f g = (f . fst) &&& (g . snd)

braidProduct :: Cartesian k
  => Product k a b `k` Product k b a
braidProduct = snd &&& fst

associateProduct :: Cartesian k
  => Product k (Product k a b) c `k` Product k a (Product k b c)
associateProduct = (fst . fst) &&& first snd

disassociateProduct :: Cartesian k
  => Product k a (Product k b c) `k` Product k (Product k a b) c
disassociateProduct =
    braid
  . second braid
  . associateProduct
  . first braid
  . braid

class (Monoidal k (Sum k), Symmetric k (Sum k))
  => CoCartesian (k :: x -> x -> *) where
  type Sum k :: x -> x -> x
  inl    :: a `k` Sum k a b
  inr    :: b `k` Sum k a b
  codiag :: Sum k a a `k` a
  (|||) :: k a c -> k b c -> Sum k a b `k` c
  infixr 2 |||

instance CoCartesian (->) where
  type Sum (->) = Either
  inl = Left
  inr = Right
  codiag = \case
    Left  a -> a
    Right a -> a
  f ||| g = \case
    Left  a -> f a
    Right b -> g b

instance Cartesian k => CoCartesian (Dual k) where
  type Sum (Dual k) = Product k
  inl     = Dual fst
  inr     = Dual snd
  codiag  = Dual diag
  f ||| g = Dual $ runDual f &&& runDual g

bimapSum :: CoCartesian k => k a c -> k b d -> Sum k a b `k` Sum k c d
bimapSum f g = (inl . f) ||| (inr . g)

braidSum :: CoCartesian k => Sum k a b `k` Sum k b a
braidSum = inr ||| inl

associateSum :: CoCartesian k => Sum k (Sum k a b) c `k` Sum k a (Sum k b c)
associateSum = braid . first braid . disassociateSum . second braid . braid

disassociateSum :: CoCartesian k => Sum k a (Sum k b c) `k` Sum k (Sum k a b) c
disassociateSum = (inl . inl) ||| first inr

