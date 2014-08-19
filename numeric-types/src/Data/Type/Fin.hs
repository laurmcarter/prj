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

module Data.Type.Fin where

import Data.Type.Nat
import Data.Constraint.Extra

data Fin (x :: N) (y :: N) where
  Fin :: Lt x y -> Fin x y

instance Proof (Fin x y) where
  type ProofOf (Fin x y) = (x < y, KnownNat x, KnownNat y)
  type Residue (Fin x y) = EmptyC
  Fin x // r = r \\ LeKnown x

weaken1 :: forall x y. Fin x y -> Fin x (S y)
weaken1 (Fin lt) = Fin $ LeS

{-
tighten1 :: Fin (S x) -> Either (Nat x) (Fin x)
tighten1 x = case x of
  FZ    -> Left  nat
  FS x' -> Right x'
-}

