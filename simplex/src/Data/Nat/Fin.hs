{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Data.Nat.Fin where

import Data.Vec.Unary

import Prelude hiding (Enum(..))
import qualified Prelude as P
-- import Prelude.SafeEnum
import Control.Arrow (first)
import Data.Proxy

data Fin (n :: UN) where
  FZ :: Nat n => Fin (S n)
  FS :: Nat n => Fin (S n) -> Fin (S (S n))

fin :: (Num a, P.Enum a) => Fin n -> (a,a)
fin n = case n of
  FZ -> (0,nat $ asProxy n)
  FS n' -> first P.succ $ fin n'

instance AsProxy (Fin n) n

instance Eq (Fin n) where
  FZ == FZ = True
  FS x == FS y = x == y
  _ == _ = False

instance Nat n => Show (Fin n) where
  showsPrec d n = showParen (d > 10)
    $ showString "Fin "
    . shows upper
    . showString " "
    . shows val
    where
    p :: Proxy n
    p = Proxy
    upper :: Int
    upper = nat p
    val :: Int
    val = fst $ fin n

weaken1 :: Nat n => Fin n -> Fin (S n)
weaken1 n = case n of
  FZ -> FZ
  FS n' -> FS $ weaken1 n'

widen1 :: Nat n => Fin n -> Fin (S n)
widen1 n = case n of
  FZ -> FS FZ
  FS n' -> FS $ widen1 n'

{-
tighten1 :: Nat n => Fin (S n) -> Maybe (Fin n)
tighten1 n = go n (Proxy :: Proxy n)
  where
  go :: Nat m => Fin (S m) -> Proxy m -> Maybe (Fin m)
  go n p = case prd p of
    Just p' -> undefined
    _       -> Nothing
-}

