{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module Data.Kind.N where

import Data.Type.Bool
import Data.Type.Equality hiding (apply)
import Data.Proxy
import GHC.Exts (Constraint)

import Data.Kind

data N
  = Z
  | S N
  deriving (Eq,Show)

data Nat (n :: N) where
  Z_ :: Nat Z
  S_ :: Nat n -> Nat (S n)

instance Generate Nat Z where
  auto = Z_
instance Generate Nat n
      => Generate Nat (S n) where
  auto = S_ auto

data Odd (n :: N) where
  OddS :: Even n -> Odd (S n)

instance Generate Odd (S Z) where
  auto = OddS auto
instance Generate Even   n
      => Generate Odd (S n) where
  auto = OddS auto

data Even (n :: N) where
  EvenZ :: Even Z
  EvenS :: Odd n -> Even (S n)

instance Generate Even Z where
  auto = EvenZ
instance Generate Odd     n
      => Generate Even (S n) where
  auto = EvenS auto

evenOdd :: x ∈ Nat -> x ∈ (Even ∪ Odd)
evenOdd x = case x of
  Z_    -> L $ EvenZ
  S_ x' -> case evenOdd x' of
    L e -> R $ OddS  e
    R o -> L $ EvenS o

